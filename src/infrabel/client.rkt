#lang racket/base

(provide infrabel-client%)

(require racket/tcp
         racket/class
         "interface.rkt"
         "../logger.rkt")


;; Read TCP port & host from file
(define (tcp-info file)
  (call-with-input-file
    file
    (lambda (in)
      (values (string->number (read-line in))
              (read-line in)))))


;; TCP input & output
(define-values (in out)
  (values #f #f))


;; Quickly try going through the different configs available
(define tcp-files (directory-list "resources/tcp/" #:build? #t))
(define (quick-connect (files tcp-files))
  (if (null? files)
    (begin (eprintf "tcp-connect failed~%")
           (exit))
    (let-values (((port host) (tcp-info (car files))))
      (with-handlers ((exn:fail:network?
                        (lambda (exn)
                          (eprintf "tcp-connect on ~a@~a failed~%" port host)
                          (quick-connect (cdr files)))))
                     ; connection succesful, update i/o ports
                     (let-values (((i o) (tcp-connect host port)))
                       (set! in i)
                       (set! out o))))))


;; Struct for sending a request over TCP
;; msg contains the request
;; on-response is either #f or a function that takes the response as argument
(struct request (msg on-response))


;; Several threads may be sending & requesting data over tcp,
;; so use this thread to avoid race conditions
(define communicator
  (thread (lambda ()
            (let loop ()
              ; blocks till thread receives a request
              (let* ((req (thread-receive))
                     (msg (request-msg req))
                     (on-response (request-on-response req)))
                (log/d (format "requesting ~a" msg))
                (writeln msg out)
                (flush-output out)
                (if on-response
                  (let ((response (read in)))
                    (unless (eof-object? response)
                      (log/d (format "response for ~a: ~a" msg response))
                      (on-response response)
                      (loop)))
                  (loop)))))))


;; Send a message over TCP
(define (put . args)
  (thread-send communicator
               (request args #f)))


;; Request something over tcp
(define (get . args)
  (let ((response #f)
        (responded #f))
  (thread-send
    communicator
    (request args (lambda (x) (set! response x) (set! responded #t))))
  (let wait ()
    (if responded
      response
      (begin (sleep 0.1)
             (wait))))))


;; Logging function that can be enabled
(define log/i void)
(define log/d void)


;; For interchangeability purposes, this has the exact same interface
;; as the infrabel% class in infrabel.rkt
(define infrabel-client%
  (class* object% (infrabel-interface<%>)
    (init-field (log-level 'debug))
    (super-new)

    (when log-level
      (set!-values (log/i log/d) (make-loggers 'infrabel/client)))

    (define/public (initialize setup-id (mode 'sim))
      (log/i "Initializing")
      (quick-connect)
      (put 'initialize setup-id mode))

    (define/public (initialized?)
      (get 'initialized?))

    (define/public (start)
      (void))
    (define/public (stop)
      (put 'stop))

    (define/public (add-loco id prev-segment curr-segment)
      (put 'add-loco id prev-segment curr-segment))
    (define/public (remove-loco id)
      (put 'remove-loco id))
    (define/public (get-loco-speed id)
      (get 'get-loco-speed id))
    (define/public (set-loco-speed id speed)
      (put 'set-loco-speed id speed))
    (define/public (change-loco-direction id)
      (put 'change-loco-direction id))
    (define/public (get-loco-d-block id)
      (get 'get-loco-d-block id))

    (define/public (get-switch-position id)
      (get 'get-switch-position id))
    (define/public (set-switch-position id position)
      (put 'set-switch-position id position))
    (define/public (get-switch-ids)
      (get 'get-switch-ids))

    (define/public (get-d-block-ids)
      (get 'get-d-block-ids))
    (define/public (get-d-block-statuses)
      (get 'get-d-block-statuses))))

