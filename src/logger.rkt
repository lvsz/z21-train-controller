#lang racket/base

(provide make-loggers
         start-logger
         stop-logger
         identity)

(require racket/date
         racket/file
         racket/match)

(define log-directory "logs")
(unless (directory-exists? log-directory)
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
(define (path topic date)
  (let ((dir (format "~a/~a" log-directory topic)))
    (make-directory* dir)
    (format "~a/~a.log" dir date)))

(define logs (make-hash))

;; Return relevant file, create it if necessary
(define (get-file topic date)
  (hash-ref! logs topic
             (lambda ()
               (open-output-file (path topic date) #:exists 'append))))

(define logger-thread #f)

(define (start-logger (level 'info))
  (define receiver (make-log-receiver logger level))
  (define (logging)
    (match (sync receiver)
      ((vector level msg _ topic)
       (let* ((date (date-string))
              (out1 (get-file #\. date))
              (out2 (get-file topic date))
              (s (format "[~a - ~a] {~a}\t~a~%" date (time-string) level msg)))
         (display s out1)
         (display s out2)
         (flush-output out1)
         (flush-output out2))))
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
