#lang racket/base

(provide run)

(require racket/class
         racket/function
         racket/list
         racket/set
         racket/random
         rackunit
         rackunit/text-ui
         "../../railway/setup.rkt"
         "../../railway/railway.rkt"
         "../../railway/tracks.rkt")

(define railway (make-object railway% 'hardware))

(define basic-tests
  (test-suite
    "Check basic methods of railway%"
    (test-case
      "Tracks are valid & accessible"
      (check-pred pair? (send railway get-tracks))
      (check-true (andmap track? (send railway get-tracks))))
    (test-case
      "Detection blocks are valid & accessible"
      (check-pred pair? (send railway get-d-blocks))
      (check-true (andmap d-block? (send railway get-d-blocks))))
    (test-case
      "Switches are valid & accessible"
      (check-pred pair? (send railway get-switches))
      (check-true (andmap switch? (send railway get-switches))))))

(define d-1-3 (send railway get-track '1-3))
(define d-1-4 (send railway get-track '1-4))
(define d-1-5 (send railway get-track '1-5))
(define d-1-8 (send railway get-track '1-8))
(define d-2-4 (send railway get-track '2-4))
(define d-2-6 (send railway get-track '2-6))
(define d-2-7 (send railway get-track '2-7))
(define d-2-8 (send railway get-track '2-8))
(define s-20 (send railway get-track 'S-20))
(define s-2 (send railway get-track 'S-2))
(define s-3 (send railway get-track 'S-3))
(define u-5 (send railway get-track 'U-5))
(define u-6 (send railway get-track 'U-6))

(define route-tests
  (test-suite
    "Check path finding methods"
    (check-equal? (map (lambda (x) (send (get-field superior x) get-id))
                       (send railway get-route d-1-4 d-2-4))
                  '(1-4 1-5 S-20 2-4)
                  "Simple path between 1-4 and 2-4")
    (check-false  (send railway get-route d-1-3 d-2-8)
                  "No valid route between 1-3 and 2-8")
    (check-exn    exn:fail?
                  (lambda () (send railway get-route u-5 d-1-4))
                  "Only accept d-blocks are start/end tracks")
    (check-equal? (map (lambda (x) (send (get-field superior x) get-id))
                       (send railway get-route d-2-7 d-1-5))
                  '(2-7 S-4 S-8 S-3 U-6 S-1 2-1 S-1 U-6
                    S-2 S-7 S-5 S-6 S-20 2-4 S-20 1-5)
                  "More complicated route requiring reversing on d-blocks")))

(define (run)
  (run-tests basic-tests)
  (run-tests route-tests))

