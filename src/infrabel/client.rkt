#lang racket/base

(provide infraclient%)

(require racket/async-channel
         racket/class
         racket/match
         racket/tcp
         racket/os
         "infraface.rkt"
         "message.rkt"
         "../logger.rkt"
         "../resources.rkt")


;; Logging functions
(define-loggers 'infrabel/client log/w log/i log/d)


;; Struct that facilitates sending requests in need of a response
;; `header` contains the method name
;; `body` contains the arguments
;; `on-response` is either #f or a function that takes the response as argument
(struct request (header body response) #:constructor-name new-request)

;; Read TCP port & host from file
(define (tcp-info file)
  (call-with-input-file
    file
    (lambda (in)
      (values (string->number (read-line in))
              (read-line in)))))


;; Quickly try going through the different configs available
;; Exits if it runs out of tries
(define (quick-connect #:files (files tcp-files)
                       #:port  (port #f)
                       #:host  (host (gethostname))
                       #:tries (tries 5))
  (define (_connect port host cont)
    (with-handlers
      ((exn:fail:network? ; Call the following if connection failed
         (lambda (exn)
           (log/i (format "TCP connection to ~a@~a failed" port host)))))
      (let-values (((in out) (tcp-connect host port)))
        ; Connection succesful, return i/o ports
        (log/i (format "TCP connection established at ~a@~a" port host))
        (file-stream-buffer-mode out 'none)
        (cont in out))))
  (define port/host
    (if port
      (hash port host)
      (for/hash ((file (in-list files)))
        (tcp-info file))))
  (let/cc
    return
    ; Give each port in `port/host` `tries` tries before giving up
    (for* ((i (in-range 1 (add1 tries)))
           ((port host) (in-hash port/host)))
      (log/i (format "TCP connection to ~a@~a, attempt ~a" port host i))
      (_connect port host return)
      (sleep 1))
    (log/w "Failed to establish TCP connection")
    (eprintf "tcp-connect failed~%")
    (exit)))


;; For interchangeability purposes, this has the exact same interface
;; as the infrabel% class in infrabel.rkt
(define infraclient%
  (class* object% (infraface<%>)
    (init-field (port #f) (host (gethostname)) (log-level 'warning))
    (super-new)

    (start-logger log-level)

    (define-values (tcp-in tcp-out)
      (quick-connect #:port port #:host host))

    (define update-channel (make-async-channel))
    (define (send-update update)
      (async-channel-put update-channel update))
    (define/public (get-update)
      update-channel)

    ; Generate new identifiers using a closure
    (define gen-id (let ((n 0))
                     (lambda () (set! n (add1 n)) n)))

    ; Several threads may be sending & requesting data over TCP
    ; The main purpose of this thread is to keep them organised
    (define (client-loop)
      (define pending (make-hash))
      (define (handle-message msg)
        (log/d "Received update:" msg)
        (match msg
          ; Response to a pending request
          ((message id 'response body)
           ; Try to retrieve the pending callback function, call it with `body`
           ((hash-ref pending id (lambda () (log/w msg "not in" pending))) body)
           (hash-remove! pending id))
          ; State update from infrabel
          ((message id 'update body)
           (send-update (cons id body)))
          ; Server severed connection
          ((or (? eof-object?) (message 'kill _ _))
           (log/w "Disconnected from server with message:" msg)
           (send-update '(kill unexpected)))
          (_ (log/w "Cannot recognize received update:" msg))))
      (define (request req)
        (let* ((id (gen-id))
               (msg (message id (request-header req) (request-body req))))
          (log/d "Sending server request" msg)
          (write msg tcp-out)
          (cond ((request-response req)
                 (hash-set! pending id (request-response req)))
                ((eq? 'stop (request-header req))
                 (send-update '(kill expected))))))
      (with-handlers
        ((exn:fail:network? (lambda (exn)
                              (log/w "Network error:" exn)
                              (handle-message eof))))
        (let loop () ; Looping part of `client-loop`
          ; Blocks until server sent something or thread received something
          (match (sync (choice-evt tcp-in (thread-receive-evt)))
            ; When a TCP port synchronizes, it returns itself
            ((? input-port?) (handle-message (read tcp-in)))
            ; Otherwise assume it's a thread message
            (_ (request (thread-receive))))
          (log/d "Pending responses:" pending)
          (loop))))

    (define client-thread
      (thread client-loop))

    ; Any message to the server should be sent using this function
    (define (communicate header body (callback #f))
      (thread-send client-thread
                   (new-request header body callback)))

    ; Send a message over TCP
    (define (put header . body)
      (communicate header body))

    ; Request something over TCP
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
    (define/public (get-d-block-status id)
      (get 'get-d-block-status id))

    (log/i "Connecting to server")
    (put 'connect)))

