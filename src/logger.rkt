#lang racket

(provide define-loggers
         set-loggers!
         start-logger
         stop-logger)

(require racket/date
         racket/file
         racket/match
         (for-syntax racket/base))

(define logger-thread #f)

(define log-directory "logs")

;; Make use of Racket logging functionality
(define logger (make-logger))


(define-syntax (define-loggers stx)
  (syntax-case stx ()
    ((_ log-w log-i log-d)
     #'(begin
         (define log-w void)
         (define log-i void)
         (define log-d void)))))

;; Loggers for logging the logger
(define-loggers log/w log/i log/d)


;; Set 3 identifiers to loggers to loggers for different levels
;; The levels are 'warning, 'info and 'debug respectively
(define-syntax (set-loggers! stx)
  (syntax-case stx ()
    ((_ topic log-w log-i log-d)
     #'(begin
         (let ((mklog (lambda (l t)
                        (lambda msg
                          (log-message logger l t (msg->string msg))))))
           (set! log-w (mklog 'warning topic))
           (set! log-i (mklog 'info    topic))
           (set! log-d (mklog 'debug   topic)))))))


;; Returns the log file's path for a given topic,
;; creating a new directory if necessary.
(define (path topic date)
  (let ((dir (format "~a/~a" log-directory topic)))
    (make-directory* dir)
    (format "~a/~a.log" dir date)))


;; Hash containing open output files for logging
(define logs (make-hash))


;; Starts the logger, creating files & directories where needed
(define (start-logger (level 'warning))
  (define receiver (make-log-receiver logger level))

  ; Return relevant file, create it if necessary
  (define (get-file topic date)
    (hash-ref! logs
               topic
               (lambda ()
                 (open-output-file (path topic date) #:exists 'append))))

  (define (logging)
    (match (choice-evt (sync receiver) (thread-receive-evt))
      ((vector level msg _ topic)
       (let* ((date (date-string))
              (out1 (get-file #\. date))
              (out2 (get-file topic date))
              (s (format "[~a - ~a] {~a}\t~a~%" date (time-string) level msg)))
         (display s out1)
         (display s out2)
         (flush-output out1)
         (flush-output out2)))
      ((? evt?)
       (match (thread-try-receive)
         ('kill (kill-thread (current-thread)))
         (#f    (log/w "Logger received unkown event"))
         (datum (log/w "Logger thread received unkown message: " datum))))
      (datum
       (log/w "Logger receiver received unkown message: " datum)))
    (logging))

  (unless (directory-exists? log-directory)
    (make-directory log-directory))

  (if logger-thread
    (log/d "'start-logger' called on running logger")
    (begin (set-loggers! 'logger log/w log/i log/d)
           (set! logger-thread (thread logging))
           (log/i "Logger started"))))


(define (stop-logger)
  (if logger-thread
    (begin (log/i "Stopping logger")
           (thread-send logger-thread 'kill)
           (set! logger-thread #f)
           (for ((file (in-hash-values logs)))
             (close-input-port file)))
    (begin (start-logger 'warning)
           (log/w "Logger already stopped")
           (stop-logger))))


;; Turns a number into a string,
;; prefixing it with a zero if it's a single digit.
(define (pad0 n)
  (if (<= 0 n 9)
    (string-append "0" (number->string n))
    (number->string n)))


;; get current time and format it for logging
(define (time-string)
  (let ((current-time (current-date)))
    (format "~a:~a:~a"
            (pad0 (date-hour current-time))
            (pad0 (date-minute current-time))
            (pad0 (date-second current-time)))))


;; Get current date and format it for logging
(define (date-string)
  (match (current-date)
    ((date* _ _ _ d m y _ _ _ _ _ _)
     (format "~a-~a-~a" y (pad0 m) (pad0 d)))))


;; Turns a list into a string, seperated with spaces by default
(define (msg->string
          msg
          #:prefix   (prefix   "")
          #:interfix (interfix " ")
          #:suffix   (suffix   ""))
  (if (null? msg)
    suffix
    (string-append
      prefix
      (format "~a" (car msg))
      (msg->string (cdr msg)
                   #:prefix   interfix
                   #:interfix interfix
                   #:suffix   suffix))))

