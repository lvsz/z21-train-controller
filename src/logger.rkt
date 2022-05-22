#lang racket/base

(provide make-loggers
         start-logger
         stop-logger
         identity)

(require racket/date
         racket/match)

(unless (directory-exists? "logs")
  (make-directory "logs"))

;; Turns a number into a string,
;; prefixing it with a zero if it's a single digit.
(define (pad0 n)
  (if (<= 0 n 9)
    (string-append "0" (number->string n))
    (number->string n)))

(define (time-string)
  (let ((current-time (current-date)))
    (format "~a:~a:~a"
            (pad0 (date-hour current-time))
            (pad0 (date-minute current-time))
            (pad0 (date-second current-time)))))

(define (date-string)
  (match (current-date)
    ((date* _ _ _ d m y _ _ _ _ _ _)
     (format "~a-~a-~a" y (pad0 m) (pad0 d)))))

(define logger
  (make-logger))

;; Returns two functions,
;; the first one to log info messages for the given topic,
;; the second one to log debug messages for the given topic.
(define (make-loggers topic)
  (define (info msg)
    (log-message logger 'info topic (format "~a" msg))
    msg)
  (define (debug msg)
    (log-message logger 'debug topic (format "~a" msg))
    msg)
  (values info debug))

;; Identity function can be used as a placeholder for logging functions.
(define identity (lambda (x) x))

;; Returns the log file's path for a given topic,
;; creating a new directory if necessary.
;: A new log file is created for each day.
(define (path topic)
  (let ((dir (format "logs/~a" topic)))
    (unless (directory-exists? dir)
      (make-directory dir))
    (format "~a/~a.txt" dir (date-string))))

(define logs (make-hash))

;; Return relevant file, create it if necessary
(define (get-file topic)
  (hash-ref! logs topic
             (lambda ()
               (open-output-file (path topic) #:exists 'append))))

(define logger-thread #f)

(define (start-logger (level 'info))
  (define receiver (make-log-receiver logger level))
  (define (logging)
    (match (sync receiver)
      ((vector level msg _ topic)
       (let ((out (get-file topic)))
         (fprintf out "~a (~a) \t~a~%" (time-string) level msg)
         (flush-output out))))
    (logging))
  (if logger-thread
    (error "start-logger: logger already running")
    (set! logger-thread (thread logging))))

(define (stop-logger)
  (if logger-thread
    (begin (kill-thread logger-thread)
           (set! logger-thread #f)
           (for ((file (in-hash-values logs)))
             (close-input-port file)))
    (error "stop-logger: logger not running")))
