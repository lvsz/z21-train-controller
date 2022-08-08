#lang racket/base

(provide infrabel-client%)

(require racket/async-channel
         racket/class
         racket/match
         racket/tcp
         "interface.rkt"
         "../logger.rkt")


;; Logging function that can be enabled
(define log/i void)
(define log/d void)

(define tcp-files (directory-list "resources/tcp/" #:build? #t))


;; Struct for sending a request over TCP
;; body contains the arguments
;; on-response is either #f or a function that takes the response as argument
(struct request (body response) #:constructor-name new-request)

;; Read TCP port & host from file
(define (tcp-info file)
  (call-with-input-file
    file
    (lambda (in)
      (values (string->number (read-line in))
              (read-line in)))))


;; Quickly try going through the different configs available
(define (quick-connect (files tcp-files))
  (let/cc
    return
    (if (null? files)
      (begin (eprintf "tcp-connect failed~%")
             (exit))
      (let-values (((port host) (tcp-info (car files))))
        (with-handlers
          ((exn:fail:network?
             (lambda (exn)
               (begin0 (quick-connect (cdr files))
                       (eprintf "tcp-connect on ~a@~a failed~%" port host)))))
          ; connection succesful, update i/o ports
          (let-values (((in out) (tcp-connect host port)))
            (return in out)))))))


;; For interchangeability purposes, this has the exact same interface
;; as the infrabel% class in infrabel.rkt
(define infrabel-client%
  (class* object% (infrabel-interface<%>)
    (super-new)
    (init (log-level 'debug))

    (when log-level
      (set!-values (log/i log/d) (make-loggers 'infrabel/client)))

    (define-values (tcp-in tcp-out)
      (quick-connect))

    (define update-channel (make-async-channel))
    (define/public (get-update)
      update-channel)

    ;; Several threads may be sending & requesting data over tcp,
    ;; the main purpose of this thread is to keep them organised
    (define (client-loop)
      (define pending (make-hash))
      (define (update args)
        (log/d "Received update:" args)
        (case (car args)
          ((loco-speed switch)
           (async-channel-put update-channel args))
          (else
           (match (hash-ref pending (car args) #f)
             (#f (log/i "Cannot not recognize received update:" args))
             (fn (fn (cdr args))
                 (hash-remove! pending (car args)))))))
      (define (request req)
        (let ((msg (request-body req))
              (response (request-response req))
              (id (string->symbol (symbol->string (gensym))))) ; force intern
          (log/d "Received request" msg)
          (write (cons id msg) tcp-out)
          (flush-output tcp-out)
          (when response
            (hash-set! pending id response))))
      (let loop ()
        (match (sync (choice-evt tcp-in (thread-receive-evt)))
          ((? input-port?) (update (read tcp-in)))
          (_ (request (thread-receive))))
        (log/d (hash-count pending) "pending responses.")
        (loop)))

    (define client-thread
      (thread client-loop))

    ;; Any message to the server should be sent using this function
    (define (communicate msg (callback #f))
      (thread-send client-thread
                   (new-request msg callback)))

    ;; Send a message over TCP
    (define (put . args)
      (communicate args))

    ;; Request something over tcp
    (define (get . args)
      (define (respond-to ch)
        (lambda (r)
          ;(unless (eof-object? response)
          (channel-put ch r)))
      (let ((ch (make-channel)))
        (communicate args (respond-to ch))
        (let ((response (channel-get ch)))
          (log/d "Response:" response)
          response)))


    (define/public (initialize setup-id (mode 'sim))
      (log/i "Initializing")
      (put 'initialize setup-id mode))

    (define/public (initialized?)
      (get 'initialized?))

    ;; Server already starts infrabel
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

