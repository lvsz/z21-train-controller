#lang racket/base

(provide run)

(require racket/class
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

(define component-tests
  (test-suite
    "Check interdependency of components"
    (send d-1-5 occupy)
    (check-eq? (send d-1-4 get-status)
               'orange
               "A detection block should turn orange if a neighbor turns red")
    (check-eq? (send s-2 get-position)
               1)))

(define (run)
  (run-tests basic-tests)
  (run-tests component-tests))

