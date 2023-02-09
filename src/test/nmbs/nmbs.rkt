#lang racket/base

(provide run)

(require racket/class
         racket/async-channel
         rackunit
         rackunit/text-ui
         "../infrabel/mock.rkt"
         "../../nmbs/nmbs.rkt"
         "../../railway/railway.rkt")


(define setup 'hardware)
(define alt-setup 'loop)

(define nmbs #f)
(define infrabel #f)
(define railway #f)
(define loco #f)

(define basic-tests
  (test-suite
    "Setting up server with mock infrabel"
    #:before (lambda ()
               (set! infrabel (new infrabel-mock%))
               (set! nmbs (new nmbs% (infrabel infrabel))))
    #:after (lambda ()
              (set! nmbs #f)
              (set! infrabel #f)
              (set! railway #f)
              (set! loco #f))
    (test-case
      "Should get the correct setup on start"
      (send nmbs start #:setup-id setup #:gui? #f)
      (set! railway (send infrabel test-railway))
      (check memq 'S-16 (send nmbs get-switch-ids))
      (check memq '2-8 (send nmbs get-d-block-ids)))
    (test-case
      "Locos can be added"
      (let ((spots (send nmbs get-starting-spots)))
        (check-pred pair? spots)
        (set! loco (send nmbs add-loco (car spots)))
        (check-eq? 0 (send nmbs get-loco-speed loco))
        (check-eq? (car spots)
                   (send nmbs get-loco-d-block loco))))
    (test-case
      "Loco speeds can be changed"
      (send nmbs set-loco-speed loco 100)
      (check-eq? 100 (send nmbs get-loco-speed loco))
      (check-eq? 100 (send infrabel get-loco-speed loco)))
    (test-case
      "Locos can change direction"
      (send nmbs change-loco-direction loco)
      (check-eq?  100 (send nmbs get-loco-speed loco))
      (check-eq? -100 (send infrabel get-loco-speed loco)))
    (test-case
      "Loco speeds can get updated by infrabel"
      (send infrabel test-set-loco-speed loco 0)
      (check-eq? 0 (send nmbs get-loco-speed loco))
      (send infrabel test-set-loco-speed loco -20)
      (check-eq? 20 (send nmbs get-loco-speed loco)))))

(define (run)
    (run-tests basic-tests))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run))

