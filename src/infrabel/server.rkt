#lang racket/base

(provide start-server)

(require racket/async-channel
         racket/class
         racket/date
         racket/match
         racket/tcp
         "infrabel.rkt"
         "interface.rkt"
         "../logger.rkt")

(define current-infrabel (make-parameter #f))
(define current-listener (make-parameter #f))
(define current-tcp-input (make-parameter (current-input-port)))
(define current-tcp-output (make-parameter (current-output-port)))

(define log/i void)
(define log/d void)

;; Try to fail as gracefully as possible
(define (stop exn)
  (if (current-infrabel)
    (send (current-infrabel) stop)
    (log/i "No infrabel parameter set"))
  (if (current-listener)
    (begin (tcp-close (current-listener))
           (close-input-port (current-tcp-input))
           (close-output-port (current-tcp-output)))
    (log/i "No listener parameter set"))
  (cond ((eq? exn 'request)
         (log/i "Infrabel server stopped by request"))
        ((exn:break? exn)
         (log/i "Infrabel server stopped by user break"))
        ((eof-object? exn)
         (log/i "Infrabel server stopped by client disconnection"))
        (else
         (log/i (format "Infrabel server stopped by unkown cause: ~a" exn))))
  (exit))


;; Receive and log message
(define (get (input (current-tcp-input)))
  (let ((msg (read input)))
    (log/d (format "received \"~a\"" msg))
    msg))

;; Reply and log message
(define (put id response (output (current-tcp-output)))
  (log/d (format "sent \"~a\"" response))
  (write (cons id response) output)
  (flush-output output))


;; Parse any received messages and respond appropriately
(define (run (infrabel (current-infrabel)))
  (when (not (is-a? infrabel infrabel-interface<%>))
    (error "run: no valid infrabel object provided"))
  ;(let loop ((msg (get)))
  (define (tcp-handler msg)
    (when (eof-object? msg)
      (stop msg))
    (let ((id (car msg))
          (method (cadr msg))
          (args (cddr msg)))
      (case method
        ((add-loco)
         (send/apply infrabel add-loco args))
        ((remove-loco)
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
        ((initialized?)
         (put id (send infrabel initialized?)))
        ((stop)
         (stop 'request))
        (else (log/i (format "Unrecongized message: ~a" msg))))))
  (define (update-handler datum)
    (put (car datum) (cdr datum)))
  (let loop ()
    (match (sync (choice-evt (send infrabel get-update) (current-tcp-input)))
      ((? input-port? tcp-in) (tcp-handler (get tcp-in)))
      (datum (update-handler datum)))
    (loop)))


;; Initialize everything and start the server
(define (start-server port
                      #:log   (log-level 'info)
                      #:setup (setup #f)
                      #:mode  (mode #f))
  (displayln (format "Server accepting TCP connections on port ~a" port))
  (define listener (tcp-listen port 4 #t))
  (define-values (tcp-in tcp-out)
    (tcp-accept listener))
  ;; Accept TCP listeners and wait until an initialize command
  (define (init-args (msg (get tcp-in)))
    (case (cadr msg)
      ((initialize)
       (log/i "Connection established on port" port)
       (if setup
         (if mode
           (list setup mode)
           (list setup))
         (cddr msg)))
      (else
       (log/i "Unexpected message:" msg)
       (init-args (get tcp-in)))))
  (define update-channel (make-channel))
  (when log-level
    (set!-values (log/i log/d) (make-loggers 'infrabel/server))
    (start-logger log-level))
  (log/i "Infrabel server activated")
  (log/i "Log level" log-level)
  (let ((infrabel (new infrabel% (log-level log-level))))
    (parameterize ((current-listener listener)
                   (current-infrabel infrabel)
                   (current-tcp-input tcp-in)
                   (current-tcp-output tcp-out))
      (with-handlers ((exn? stop))
                     (send/apply infrabel initialize (init-args))
                     (begin0 (send infrabel start)
                             (log/i "Infrabel started")
                             (thread run))))))

