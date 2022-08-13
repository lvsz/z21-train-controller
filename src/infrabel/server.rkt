#lang racket/base

(provide start-server)

(require racket/async-channel
         racket/class
         racket/date
         racket/match
         racket/set
         racket/tcp
         "infrabel.rkt"
         "interface.rkt"
         "message.rkt"
         "../logger.rkt")


;; Logging function that can be enabled
(define-loggers log/w log/i log/d)


;; Receive and log message
(define (input-from input)
  (let ((msg (read input)))
    (log/d (format "received \"~a\"" msg))
    msg))

;; Reply and log message
(define (output-to id response output)
  (let ((msg (message id 'response response)))
    (log/d (format "sent \"~a\"" msg))
    (write msg output)))


(define (new-client-thread infrabel tcp-in tcp-out)
  (define (put id response)
    (output-to id response tcp-out))

  (define (get)
    (input-from tcp-in))

  ; Keep track of locos in case of disconnect
  (define locos (mutable-set))

  (define (stop msg)
    (for ((loco (in-set locos)))
      (send infrabel remove-loco loco))
    (tcp-abandon-port tcp-in)
    (tcp-abandon-port tcp-out)
    (log/i "Connection with client ended:" msg)
    (kill-thread (current-thread)))

  (define (tcp-handler msg)
    (when (eof-object? msg)
      (stop 'disconnect))
    (let ((id     (message-id msg))
          (method (message-header msg))
          (args   (message-body msg)))
      (case method
        ((add-loco)
         (set-add! locos (car args))
         (send/apply infrabel add-loco args))
        ((remove-loco)
         (set-remove! locos (car args))
         (send/apply infrabel remove-loco args))
        ((get-loco-speed)
         (put id (send/apply infrabel get-loco-speed args)))
        ((set-loco-speed)
         (send/apply infrabel set-loco-speed args))
        ((change-loco-direction)
         (send/apply infrabel change-loco-direction args))
        ((get-loco-d-block)
         (put id (send/apply infrabel get-loco-d-block args)))
        ((get-switch-position)
         (put id (send/apply infrabel get-switch-position args)))
        ((set-switch-position)
         (send/apply infrabel set-switch-position args))
        ((get-switch-ids)
         (put id (send infrabel get-switch-ids)))
        ((get-d-block-ids)
         (put id (send infrabel get-d-block-ids)))
        ((get-d-block-statuses)
         (put id (send infrabel get-d-block-statuses)))
        ((get-setup)
         (put id (send infrabel get-setup)))
        ((initialize)
         (send/apply infrabel initialize args))
        ((start)
         (send infrabel start))
        ((stop)
         (stop 'request))
        (else (log/i "Unrecongized message: ~a" msg)))))

  (define (update-handler datum)
    (when (or (not (eq? (car datum) 'loco-speed))
              (set-member? (cadr datum) locos))
      (put (car datum) (cdr datum))))

  (define (client-loop)
    ; If a TCP port synchronizes, it returns the port
    ; So it's not a port, it's an update
    (if (input-port? (sync (choice-evt tcp-in (thread-receive-evt))))
      (tcp-handler (get))
      (update-handler (thread-receive)))
    (client-loop))
  (thread client-loop))


;; Initialize everything and start the server
(define (start-server port
                      #:log   (log-level 'info)
                      #:setup (setup #f))
  (define listener (tcp-listen port))
  (define client-threads '())
  (define infrabel (new infrabel% (log-level log-level)))

  (define master-thread (current-thread))
  (define run-thread (current-thread))

  (define (run)
    ; Accepting clients and updating them both utilise the client-threads list
    ; To prevent race conditions, these get handled in the same thread
    (match (sync (choice-evt (tcp-accept-evt listener)
                             (send infrabel get-update)))
      ; If tcp-accept-evt synchronized, it returns two TCP ports
      ((list (? tcp-port? tcp-in) tcp-out)
       (let ((msg (input-from tcp-in)))
         (file-stream-buffer-mode tcp-out 'none)
         (if (and (message? msg)
                  (eq? (message-header msg) 'connect))
           (begin (set! client-threads (cons (new-client-thread
                                               infrabel
                                               tcp-in
                                               tcp-out)
                                             client-threads))
                  (log/i "Connection established on port" port))
           (log/d "Expected \"connect\" message but received:" msg))))
      ; Else it's an infrabel update for the threads to handle
      ; Also remove any threads that are no longer running
      (update
       (set! client-threads (map (lambda (c) (thread-send c update))
                                 (filter thread-running? client-threads)))))
    (run))

  (define (stop exn)
    (log/i "Server shutting down.")
    ; Kill run loop and client threads
    (for-each kill-thread (cons run-thread client-threads))
    (send infrabel stop)
    (cond ((eq? exn 'request)
           (log/i "Infrabel server stopped by request"))
          ((exn:break? exn)
           (log/i "Infrabel server stopped by user break"))
          (else
           (log/i "Infrabel server stopped by unkown cause:" exn)))
    (tcp-close listener)
    (kill-thread master-thread))

  (define (repl)
    (displayln "Enter 'exit' to stop server.")
    (display "> ")
    (if (string=? (read-line) "exit")
      (stop 'request)
      (begin (displayln "Command not recognized.")
             (repl))))

  (with-handlers
    ((exn? stop))

    (when log-level
      (set-loggers! 'infrabel/server (log/w log/i log/d)))
      (start-logger log-level))

    (when setup
      (send infrabel initialize setup))

    (displayln (format "Server accepting TCP connections on port ~a." port))
    (thread repl)
    (let-values (((in out) (tcp-accept listener)))
      (file-stream-buffer-mode out 'none)
      (set! client-threads (list (new-client-thread infrabel in out))))

    (log/i "Infrabel server activated")
    (log/i "Log level" log-level)

    ; Blocks until infrabel is initialized
    (sync (send infrabel get-update))

    (begin0 (send infrabel start)
            (log/i "Infrabel started")
            (set! run-thread (thread run)))))

