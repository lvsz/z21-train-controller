#lang racket/base

(provide start-server)

(require racket/tcp
         racket/date
         racket/class
         "infrabel.rkt"
         "../logger.rkt")

(define infrabel #f)

(define listener #f)
(define in #f)
(define out #f)


;; Try to fail as gracefully as possible
(define (stop exn)
  (send infrabel stop)
  (when listener
    (tcp-close listener)
    (and in (tcp-abandon-port in))
    (and out (tcp-abandon-port out)))
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
(define (get-msg)
  (let ((msg (read in)))
    (log/d (format "received \"~a\"" msg))
    msg))

;; Reply and log message
(define (reply response)
  (log/d (format "sent \"~a\"" response))
  (writeln response out)
  (flush-output out))


;; Parse any received messages and respond appropriately
(define (run)
  (let loop ((msg (get-msg)))
    (when (eof-object? msg)
      (stop msg))
    (let ((method (car msg))
          (args (cdr msg)))
      (case method
        ((add-loco)
         (send/apply infrabel add-loco args))
        ((remove-loco)
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
        ((get-d-block-statuses)
         (reply (send infrabel get-d-block-statuses)))
        ((initialized?)
         (reply (send infrabel initialized?)))
        ((stop)
         (stop 'request))
        (else (log/i (format "Unrecongized message: ~a" msg))))
      (loop (get-msg)))))


(define log/i void) ; info-level logging function
(define log/d void) ; debug-level logging function


;; Initialize everything and start the server
(define (start-server port (log-level 'debug))
  ;; Accept TCP listeners and wait until an initialize command
  (define (init-args)
    (let-values (((_in _out) (tcp-accept listener)))
      (set! in _in)
      (set! out _out))
    (let loop ((msg (get-msg)))
      (case (car msg)
        ((initialize)
         (log/i (format "Connection established on port ~a" port))
         (cdr msg))
        (else
         (log/d "Unexpected message:" msg)
         (loop (get-msg))))))
  (set! listener (tcp-listen port 4 #t))
  (when log-level
    (set!-values (log/i log/d) (make-loggers 'infrabel/server))
    (start-logger log-level))
  (log/i "Infrabel server activated")
  (log/i (format "Log level ~a" log-level))
  (set! infrabel (new infrabel% (log-level log-level)))
  (with-handlers ((exn:break? stop))
                 (send/apply infrabel initialize (init-args port))
                 (begin0 (send infrabel start)
                         (log/i "Infrabel started")
                         (thread run))))

