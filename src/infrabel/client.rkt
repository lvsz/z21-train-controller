#lang racket/base

(provide infrabel-client%)

(require racket/async-channel
         racket/class
         racket/match
         racket/tcp
         "interface.rkt"
         "message.rkt"
         "../logger.rkt")


;; Logging functions
(define-loggers 'infrabel/client log/w log/i log/d)

(define tcp-files (directory-list "resources/tcp/" #:build? #t))


;; Struct that facilitates sending requests in need of a response
;; header contains the method name
;; body contains the arguments
;; on-response is either #f or a function that takes the response as argument
(struct request (header body response) #:constructor-name new-request)

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
          ; Connection succesful, update i/o ports
          (let-values (((in out) (tcp-connect host port)))
            (file-stream-buffer-mode out 'none)
            (return in out)))))))


;; Returns a unique id generator for messages
(define (id-gen)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))

;; Check whether a value could be a request id
(define request-id? integer?)


;; For interchangeability purposes, this has the exact same interface
;; as the infrabel% class in infrabel.rkt
(define infrabel-client%
  (class* object% (infrabel-interface<%>)
    (super-new)

    (define-values (tcp-in tcp-out)
      (quick-connect))

    (define update-channel (make-async-channel))
    (define (send-update update)
      (async-channel-put update-channel update))
    (define/public (get-update)
      update-channel)

    ; `id-gen` returns a lambda to generate ids
    (define gen-id (id-gen))

    ; Several threads may be sending & requesting data over TCP
    ; The main purpose of this thread is to keep them organised
    (define (client-loop)
      (define pending (make-hash))
      (define (update msg)
        (log/d "Received update:" msg)
        ; If id doesn't fit that of a request, forward to update-channel
        ; Otherwise try to fill in pending request
        (if (not (request-id? (message-id msg)))
          (send-update (cons (message-id msg) (message-body msg)))
          (let ((fn (hash-ref pending (message-id msg) #f)))
            (if fn
              (begin (fn (message-body msg))
                     (hash-remove! pending (message-id msg)))
              (log/w "Cannot not recognize received update:" msg)))))
      (define (request req)
        (let* ((id (gen-id))
               (msg (message id (request-header req) (request-body req))))
          (log/d "Sending server request" msg)
          (write msg tcp-out)
          (when (request-response req)
            (hash-set! pending id (request-response req)))))
      (let loop ()
        (match (sync (choice-evt tcp-in (thread-receive-evt)))
          ; When a TCP port synchronizes, it returns itself
          ((? input-port?) (update (read tcp-in)))
          ; Otherwise assume it's a thread message
          (_ (request (thread-receive))))
        (log/d "Pending responses:" pending)
        (loop)))

    (define client-thread
      (thread client-loop))

    ;; Any message to the server should be sent using this function
    (define (communicate header body (callback #f))
      (thread-send client-thread
                   (new-request header body callback)))

    ;; Send a message over TCP
    (define (put header . body)
      (communicate header body))

    ;; Request something over TCP
    (define (get header . body)
      (define (respond-to ch)
        (lambda (r)
          (channel-put ch r)))
      (let ((ch (make-channel)))
        (communicate header body (respond-to ch))
        ; Blocks until response is received
        (let ((response (channel-get ch)))
          (log/d "Response from server:" response)
          response)))

    (define/public (get-setup)
      (get 'get-setup))

    (define/public (initialize setup-id)
      (if (get-setup)
        (log/i "Server already initialized")
        (begin (log/i "Initializing server")
               (put 'initialize setup-id))))

    (define/public (start)
      (put 'start))
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
      (get 'get-d-block-statuses))

    (log/i "Connecting to server")
    (put 'connect)))

