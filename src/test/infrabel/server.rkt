#lang racket/base

(provide run)

(require racket/class
         racket/tcp
         racket/async-channel
         rackunit
         rackunit/text-ui
         "mock.rkt"
         "../../infrabel/server.rkt"
         "../../infrabel/message.rkt"
         "../../railway/railway.rkt"
         "../../railway/tracks.rkt")

(define-values (stdin stdout)
  (values (current-input-port) (current-output-port)))

(define-values (mock-server-stdin mock-client-stdout)
  (make-pipe))

(define-values (mock-client-stdin mock-server-stdout)
  (make-pipe))

(define setup 'hardware)
(define alt-setup 'loop)

(define infrabel (new infrabel-mock%))

(define port #f)

(define-values (in out)
  (values #f #f))

(define (put #:id (id 0) msg . args)
  (write (message id msg args) out))

(define (get-message)
  (read in))

(define (get-return-value)
  (message-body (get-message)))

(define (new-server! setup-id)
  (set! infrabel (new infrabel-mock%))
  (set! port (random 10001 62000))
  (set!-values (mock-server-stdin mock-client-stdout) (make-pipe))
  (set!-values (mock-client-stdin mock-server-stdout) (make-pipe))
  (parameterize ((current-input-port  mock-server-stdin)
                 (current-output-port mock-server-stdout))
    (thread (lambda ()
              (start-server port #:infrabel infrabel #:setup setup-id))))
  (sync mock-client-stdin)
  (set!-values (in out) (tcp-connect "localhost" port))
  (file-stream-buffer-mode out 'none))

(define (close-server!)
  (tcp-abandon-port in)
  (tcp-abandon-port out)
  (displayln 'exit mock-client-stdout))

(define no-setup-tests
  (test-suite
    "Setting up server with mock infrabel"
    #:before (lambda () (new-server! #f))
    #:after  (lambda () (close-server!))
    (test-case
      "Shouldn't have a setup before initialization"
      (check-false (begin (put 'get-setup)
                          (get-return-value)))
      "Should have a setup after initialization"
      (check-eq? (begin (put 'initialize setup)
                        (put 'start)
                        (put 'get-setup)
                        (get-return-value))
                 setup))))

(define with-setup-tests
  (test-suite
    "Setting up server with mock infrabel"
    #:before (lambda () (new-server! setup))
    #:after  (lambda () (close-server!))
    (test-case
      "Should have a setup before initialization"
      (check-eq? (begin (put 'get-setup)
                        (get-return-value))
                 setup))
    (test-suite
      "Testing switches"
      (test-case
        "Switch should change position"
        (check-not-eq?
          (let ((pos (begin (put 'get-switch-position 'S-5)
                            (get-return-value))))
            (put 'set-switch-position 'S-5 (add1 (modulo pos 2)))
            pos)
          (begin (put 'get-switch-position 'S-5)
                 (get-return-value)))))
    (test-suite
      "Testing locos"
      (test-case
        "Should add loco"
        (check-eq? (begin (put 'add-loco 'L1 '1-4 '1-5)
                          (put 'get-loco-d-block 'L1)
                          (get-return-value))
                   '1-5))
      (test-case
        "Should change loco speed"
        (check-eq? (begin (put 'set-loco-speed 'L1 50)
                          (put 'get-loco-speed 'L1)
                          (get-return-value))
                   50))
      (test-case
        "Should remove loco"
        (check-false (begin (put 'remove-loco 'L1)
                            (put 'get-loco-d-block 'L1)
                            (get-return-value)))))
    (test-suite
      "Test update broadcast"
      (test-case
        "Should forward infrabel update"
        (check-eq? (begin (async-channel-put (send infrabel get-update)
                                             '(switch 'S-1 2))
                          (message-header (get-message)))
                   'update)))))


(define (run)
    (run-tests no-setup-tests)
    (run-tests with-setup-tests))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run))

