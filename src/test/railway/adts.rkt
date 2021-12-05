#lang racket/base

(provide run)

(require racket/class
         racket/set
         rackunit
         rackunit/text-ui
         "../../railway/adts.rkt")

(define n1 (make-object node% 'n1))
(define n2 (make-object node% 'n2))
(define n3 (make-object node% 'n3))
(define n4 (make-object node% 'n4))
(define n5 (make-object node% 'n5))
(define n6 (make-object node% 'n6))
(define n7 (make-object node% 'n7))
(define n8 (make-object node% 'n8))
(define n9 (make-object node% 'n9))
(define n10 (make-object node% 'n10))
(define n11 (make-object node% 'n11))
(define n12 (make-object node% 'n12))
(define nodes (list n1 n2 n3 n4 n5 n6 n7 n8 n9 n10 n11 n12))

; railway setup used for tests
;
;      n1============n2
;       \            /
;  n6----}n5--n4--n3(
;  ||   /            \
;  n7==n8            n9==n10==n11==n12
;
; double line indicates detection block
; paren indicates regular switch (2 exits)
; bracket indicates double switch (3 exits)

(define b-n1-n2 (make-object d-block% 'b-n1-n2 n1 n2 500))
(define t-n2-n3 (make-object d-block% 't-n2-n3 n2 n3 200))
(define t-n3-n4 (make-object track% 't-n3-n4 n3 n4 100))
(define t-n4-n5 (make-object track% 't-n4-n5 n4 n5 100))
(define t-n5-n1 (make-object track% 't-n5-n1 n1 n5 200)) ; order of given nodes shouldn't matter
(define t-n5-n6 (make-object track% 't-n5-n6 n5 n6 50))
(define b-n6-n7 (make-object d-block% 'b-n6-n7 n6 n7 50))
(define b-n7-n8 (make-object d-block% 'b-n7-n8 n7 n8 50))
(define s-n5<n1-n6 (make-object switch% 's-n5<n1-n6 t-n5-n1 t-n5-n6))
(define t-n5-n8 (make-object track% 't-n5-n8 n5 n8 50))
(define s-n5<n1-n6-n8 (make-object switch% 's-n5<n1-n6-n8 s-n5<n1-n6 t-n5-n8))
(define t-n3-n9 (make-object track% 't-n3-n9 n3 n9 100))
(define s-n3<n2-n9 (make-object switch% 's-n3<n2-n9 t-n2-n3 t-n3-n9))
(define b-n9-n10 (make-object d-block% 'b-n9-n10 n9 n10 50))
(define b-n10-n11 (make-object d-block% 'b-n10-n11 n10 n11 50))
(define b-n11-n12 (make-object d-block% 'b-n11-n12 n11 n12 50))
(define tracks (list b-n1-n2 t-n2-n3 t-n3-n4 t-n4-n5 t-n5-n1 t-n5-n6 b-n6-n7 b-n7-n8 s-n5<n1-n6 t-n5-n8 s-n5<n1-n6-n8 t-n3-n9 s-n3<n2-n9))

(define node-tests
  (test-suite
    "Tests for node% in tracks.rkt"
    (test-case
      "No node should have more than 2 tracks connected to it"
      (for ((node (in-list nodes)))
        (check <= (length (send node get-tracks)) 2)))))

(define track-tests
  (test-suite
    "Tests for track% in tracks.rkt"
    (test-case
      "Given a connected track, from should return either a segment on the other side or #f"
      (check-eq? (send b-n10-n11 from b-n11-n12)
                 b-n9-n10)
      (check-false (send t-n5-n1 from b-n7-n8)
                   "Return #f when tracks don't connect"))))

(define block-tests
  (test-suite
    "Tests for d-block% in tracks.rkt"
    (test-case
      "Detection blocks can update their status and that of any connected block"
      (check-eq? (send t-n2-n3 get-status)
                 'green)
      (send t-n2-n3 occupy)
      (check-eq? (send t-n2-n3 get-status)
                 'red)
      (send b-n6-n7 occupy)
      (check-eq? (send b-n6-n7 get-status)
                 'red)
      (check-eq? (send b-n7-n8 get-status)
                 'orange
                 "Block b-n7-n8 should turn orange when connected block b-n6-n7 turns red")
      (send b-n6-n7 clear)
      (check-eq? (send b-n7-n8 get-status)
                 'green
                 "Block b-n7-n8 should turn green when connected block b-n6-n7 turns green")
      (send b-n9-n10 occupy)
      (send b-n11-n12 occupy)
      (check-eq? (send b-n10-n11 get-status)
                 'orange
                 "Block b-n10-n11 has two occupied neighbours, it should be red")
      (send b-n11-n12 clear)
      (check-eq? (send b-n10-n11 get-status)
                 'orange
                 "Block b-n10-n11 should remain orange after clearing one of its neighbours"))))

(define switch-tests
  (test-suite
    "Tests for switch% in tracks.rkt"
    (test-case
      "A switch can change position"
      (check-eq? (begin (send s-n5<n1-n6 set-position 2)
                        (send s-n5<n1-n6 get-position))
                 2))))


(define (run)
  (run-tests node-tests)
  (run-tests track-tests)
  (run-tests block-tests)
  (run-tests switch-tests))

