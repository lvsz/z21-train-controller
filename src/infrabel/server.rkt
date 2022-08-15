#lang racket/base

(provide start-server)

(require racket/async-channel
         racket/class
         racket/date
         racket/match
         racket/set
         racket/tcp
         racket/os
         "infrabel.rkt"
         "interface.rkt"
         "message.rkt"
         "../logger.rkt")


;; Logging functions
(define-loggers 'infrabel/server log/w log/i log/d)


;; Receive and log message
(define (input-from input)
  (let ((msg (read input)))
    (log/d (format "Received \"~a\"" msg))
    msg))

;; Reply and log message
(define (output-to output id header body)
  (let ((msg (message id header body)))
    (log/d (format "sent \"~a\"" msg))
    (write msg output)))


;; A client% object has three public methods:
;; `(send a-client update msg)` forwards the msg to a-client
;; `(send a-client running?)` returns a boolean indicating whether it's active
;; `(send a-client kill)` kills the connection and client thread
(define client%
  (class object%
    (init-field infrabel tcp-in tcp-out)
    (super-new)

    (define-syntax put
      (syntax-rules ()
        ((_ tag id body) (output-to tcp-out id tag body))))

    (define parent-thread (current-thread))

    ; Keep track of locos in case of disconnect
    (define locos (mutable-set))

    (define (stop msg)
      (case msg
        ((disconnect request)
         (for ((loco (in-set locos)))
           (send infrabel remove-loco loco)))
        ((kill)
         (put #f 'kill #f))
        ((server)
         (thread-send parent-thread 'kill)))
      (tcp-abandon-port tcp-in)
      (tcp-abandon-port tcp-out)
      (log/i "Connection with client ended:" msg)
      (kill-thread client-thread))

    (define (tcp-handler msg)
      (define-syntax reply
        (syntax-rules ()
          ((_ m) (put 'response (message-id msg) m))))
      (when (eof-object? msg)
        (stop 'disconnect))
      (let ((method (message-header msg))
            (args   (message-body msg)))
        (case method
          ((add-loco)
           (set-add! locos (car args))
           (send/apply infrabel add-loco args))
          ((remove-loco)
           (set-remove! locos (car args))
           (send/apply infrabel remove-loco args))
          ((get-loco-speed)
           (reply (send/apply infrabel get-loco-speed args)))
          ((set-loco-speed)
           (send/apply infrabel set-loco-speed args))
          ((change-loco-direction)
           (send/apply infrabel change-loco-direction args))
          ((get-loco-d-block)
           (reply (send/apply infrabel get-loco-d-block args)))
          ((get-switch-position)
           (reply (send/apply infrabel get-switch-position args)))
          ((set-switch-position)
           (send/apply infrabel set-switch-position args))
          ((get-switch-ids)
           (reply (send infrabel get-switch-ids)))
          ((get-d-block-ids)
           (reply (send infrabel get-d-block-ids)))
          ((get-d-block-status)
           (reply (send/apply infrabel get-d-block-status args)))
          ((get-setup)
           (reply (send infrabel get-setup)))
          ((initialize)
           (send/apply infrabel initialize args))
          ((start)
           (log/i "Starting infrabel")
           (send infrabel start))
          ((stop)
           (stop 'request))
          ((stop-server)
           (stop 'server))
          (else (log/i "Unrecongized message: ~a" msg)))))

    (define (update-handler datum)
      (cond ((not (pair? datum))
             (log/w "Invalid update received:" datum))
            ((not (eq? (car datum) 'loco-speed))
             (log/d "Sending update to client:" datum)
             (put 'update (car datum) (cdr datum)))
            ((and (pair? (cdr datum))
                  (set-member? (cadr datum) locos))
             (log/d "Sending loco-speed update to client:" datum)
             (put 'update (car datum) (cdr datum)))
            (else (log/w "Invalid update received:" datum))))

    (define (client-loop)
      ; If a TCP port synchronizes, it returns the port
      ; If it's not a port, it's an update
      (if (input-port? (sync (choice-evt tcp-in (thread-receive-evt))))
        (tcp-handler (input-from tcp-in))
        (update-handler (thread-receive)))
      (client-loop))

    (define/public (update msg)
      (thread-send client-thread msg))

    (define/public (running?)
      (thread-running? client-thread))

    (define/public (kill)
      (when (running?)
        (stop 'kill)))

    ; Thread that sends messages over TCP
    (define client-thread (thread client-loop))))


;; Initialize everything and start the server
(define (start-server port
                      #:hostname  (host (gethostname))
                      #:infrabel  (infrabel #f)
                      #:setup     (setup #f)
                      #:log-level (log-level 'warning))
  (unless infrabel
    (set! infrabel (new infrabel% (log-level log-level))))

  (define listener (tcp-listen port))
  (define client-threads '())

  (define master-thread (current-thread))
  (define updater-thread (current-thread))

  (define (updater)
    (define (update-loop clients)
      ; Accepting clients and updating them with given list of client threads
      ; To prevent race conditions, these get handled in the same thread
      (match (sync (choice-evt (tcp-accept-evt listener)
                               (thread-receive-evt)
                               (send infrabel get-update)))
        ; If tcp-accept-evt synchronized, it returns two TCP ports
        ((list (? tcp-port? tcp-in) tcp-out)
         (let ((msg (input-from tcp-in)))
           (cond ((and (message? msg) (eq? (message-header msg) 'connect))
                  (log/i "Connection established on port" port)
                  ; Set buffer to 'none to flush output after each write
                  (file-stream-buffer-mode tcp-out 'none)
                  (update-loop
                    (cons (make-object client% infrabel tcp-in tcp-out)
                          clients)))
                 (else (log/w "Expected 'connect message but received:" msg)
                       (update-loop clients)))))
        ; If thread-receive-evt synchronized, it returns an event
        ((? evt?)
         (let ((msg (thread-receive)))
           (case msg
             ((kill) (log/d "Killing client threads")
                     (for-each (lambda (c) (send c kill)) clients)
                     (log/d "Killing updater thread")
                     (kill-thread (current-thread)))
             (else   (log/w "Unexpected thread message in update-loop:" msg)
                     (update-loop clients)))))
        ; Else it's an infrabel update for the threads to handle
        ; Also remove any threads that are no longer running
        (update-msg
         (log/d "Sending update to client threads:" update-msg)
         (update-loop (for/list ((client (in-list clients))
                                  #:when (send client running?))
                         (send client update update-msg)
                         client)))))
    ; Waits for first connection before entering loop
    (let-values (((tcp-in tcp-out) (tcp-accept listener)))
      (log/i "Connection established, server activated")
      (file-stream-buffer-mode tcp-out 'none)
      (update-loop (list (make-object client% infrabel tcp-in tcp-out)))))

  (define (stop exn)
    (log/i "Server shutting down.")
    ; Kill run loop and client threads
    (when (thread-running? updater-thread)
      (thread-send updater-thread 'kill))
    (send infrabel stop)
    (cond ((eq? exn 'request)
           (log/i "Infrabel server stopped by request"))
          ((exn:break? exn)
           (log/i "Infrabel server stopped by user break"))
          (else
           (log/w "Infrabel server stopped by unkown cause:" exn)))
    (when (sync/timeout 1 (thread-dead-evt updater-thread))
      (tcp-close listener))
    (kill-thread master-thread))

  ;; A "REPL" tht only accepts the 'exit' comand
  (define (repl)
    (displayln "Enter 'exit' to stop server.")
    (display "> ")
    (let ((input (read-line)))
      (if (string=? input "exit")
        (stop 'request)
        (begin (display (format "Command not recognized:~a~%" input))
               (repl)))))

  (with-handlers
    ((exn? stop))

    (when setup
      (send infrabel initialize setup))

    (set! updater-thread (thread updater))
    (display
      (format "Server accepting TCP connections on ~a@~a.~%" port host))
    (thread repl)

    (let ((update (sync (send infrabel get-update))))
      (case update
        (((initialized)) (log/i "Infrabel initialized"))
        (else (log/w "Expected '(initialized), but received:" update)))
      (send infrabel start))))

