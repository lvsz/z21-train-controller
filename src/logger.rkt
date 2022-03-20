#lang racket/base

(provide make-loggers
         start-logger
         identity)

(require racket/date
         racket/match)

(unless (directory-exists? "logs")
  (make-directory "logs"))

(define (pad0 n)
  (if (< n 10)
    (string-append "0" (number->string n))
    (number->string n)))

(define (time-string)
  (let ((current-time (current-date)))
    (format "~a:~a:~a"
            (pad0 (date-hour current-time))
            (pad0 (date-minute current-time))
            (pad0 (date-second current-time)))))

(define logr
  (make-logger))

(define (make-loggers topic)
  (define (info msg)
    (log-message logr 'info topic (format "~a" msg))
    msg)
  (define (debug msg)
    (log-message logr 'debug topic (format "~a" msg))
    msg)
  (values info debug))

;; Identity function can be used as a placeholder for logging functions
(define identity (lambda (x) x))

(define (path topic)
  (let ((dir (format "logs/~a" topic)))
    (unless (directory-exists? dir)
      (make-directory dir))
    (match (current-date)
      ((date* _ _ _ d m y _ _ _ _ _ _)
       (format "~a/~a-~a-~a.txt" dir y (pad0 m) (pad0 d))))))

(define logs (make-hash))

(define (get-file topic)
  (hash-ref! logs topic (open-output-file (path topic) #:exists 'append)))

(define (start-logger (level 'info))
  (define receiver (make-log-receiver logr level))
  (define (logging)
    (match (sync receiver)
      ((vector level msg _ topic)
       (let ((out (get-file topic)))
         (fprintf out "~a (~a) \t~a~%" (time-string) level msg)
         (flush-output out))))
    (logging))
  (thread logging))
