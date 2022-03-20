#lang racket/base

(provide infrabel%)

(require racket/tcp
         racket/class
         "../logger.rkt")


;; Read TCP port & host from file
(define-values (port host)
  (call-with-input-file
    "resources/tcp.txt"
    (lambda (file)
      (values (string->number (read-line file))
              (read-line file)))))

;; TCP input & output
(define-values (in out)
  (values #f #f))

;; Try connecting to server 10 times before failing
(define max-attempts 10)
(define (try-connect (n 1))
  (when (>= n max-attempts)
    (eprintf "tcp-connect failed after ~a attempts~%" n)
    (exit))
  (with-handlers ((exn:fail:network?
                      (lambda (exn)
                        (eprintf "tcp-connect attempt ~a failed~%" n)
                        (sleep (* n 0.5))
                        (try-connect (add1 n)))))
                 ; connection succesful, update i/o ports
                 (let-values (((i o) (tcp-connect host port)))
                   (set! in i)
                   (set! out o))))

;; struct for sending a request over tcp
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
                (debug (format "requesting ~a" msg))
                (writeln msg out)
                (flush-output out)
                (if on-response
                  (let ((response (read in)))
                    (unless (eof-object? response)
                      (debug (format "response for ~a: ~a" msg response))
                      (on-response response)
                      (loop)))
                  (loop)))))))

;; send a message over tcp
(define (put . args)
  (thread-send communicator
               (request args #f)))

;; request something over tcp
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
(define info identity)
(define debug identity)

;; for interchangeability purposes, this has the exact same interface
;; as the infrabel% class in infrabel.rkt
(define infrabel%
  (class object%
    (init-field (log-level 'debug))
    (super-new)

    (when log-level
      (set!-values (info debug) (make-loggers 'infrabel-client))
      (start-logger log-level))

    (define/public (initialize setup-id)
      (info "initializing")
      (try-connect)
      (put 'initialize setup-id))

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

