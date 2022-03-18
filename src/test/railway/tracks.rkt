#lang racket/base

(provide run)

(require racket/class
         racket/set
         rackunit
         rackunit/text-ui
         "../../railway/tracks.rkt")

(define n1  (make-object node% 'n1))
(define n2  (make-object node% 'n2))
(define n3  (make-object node% 'n3))
(define n4  (make-object node% 'n4))
(define n5  (make-object node% 'n5))
(define n6  (make-object node% 'n6))
(define n7  (make-object node% 'n7))
(define n8  (make-object node% 'n8))
(define n9  (make-object node% 'n9))
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

(define b-12 (make-object d-block% 'b-12 n1 n2 500))
(define t-23 (make-object d-block% 't-23 n2 n3 200))
(define t-34 (make-object track% 't-34 n3 n4 100))
(define t-45 (make-object track% 't-45 n4 n5 100))
(define t-51 (make-object track% 't-51 n1 n5 200))
(define t-56 (make-object track% 't-56 n5 n6 50))
(define b-67 (make-object d-block% 'b-67 n6 n7 50))
(define b-78 (make-object d-block% 'b-78 n7 n8 50))
(define s-51-56 (make-object switch% 's-51-56 t-51 t-56))
(define t-58 (make-object track% 't-58 n5 n8 50))
(define s-5156-58 (make-object switch% 's-5156-58 s-51-56 t-58))
(define t-39 (make-object track% 't-39 n3 n9 100))
(define s-23-39 (make-object switch% 's-23-39 t-23 t-39))
(define b-910 (make-object d-block% 'b-910 n9 n10 50))
(define b-1011 (make-object d-block% 'b-1011 n10 n11 50))
(define b-1112 (make-object d-block% 'b-1112 n11 n12 50))
(define tracks (list b-12 t-23 t-34 t-45 t-51
                     t-56 b-67 b-78 s-51-56 t-58
                     s-5156-58 t-39 s-23-39))

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
      "Given a connected track, from should return the next track or #f"
      (check-eq? (send b-1011 from b-1112)
                 b-910)
      (check-false (send t-51 from b-78)
                   "Return #f when tracks don't connect"))))

(define block-tests
  (test-suite
    "Tests for d-block% in tracks.rkt"
    (test-case
      "Detection blocks can update their status and that of any connected block"
      (check-eq? (send t-23 get-status)
                 'green)
      (send t-23 occupy)
      (check-eq? (send t-23 get-status)
                 'red)
      (send b-67 occupy)
      (check-eq? (send b-67 get-status)
                 'red)
      (check-eq? (send b-78 get-status)
                 'orange
                 "Block b-78 should turn orange when connected block b-67 turns red")
      (send b-67 clear)
      (check-eq? (send b-78 get-status)
                 'green
                 "Block b-78 should turn green when connected block b-67 turns green")
      (send b-910 occupy)
      (send b-1112 occupy)
      (check-eq? (send b-1011 get-status)
                 'orange
                 "Block b-1011 has two occupied neighbours, it should be red")
      (send b-1112 clear)
      (check-eq? (send b-1011 get-status)
                 'orange
                 "Block b-1011 should remain orange after clearing one of its neighbours"))))

(define switch-tests
  (test-suite
    "Tests for switch% in tracks.rkt"
    (test-case
      "A switch can change position"
      (check-eq? (begin (send s-51-56 set-position 2)
                        (send s-51-56 get-position))
                 2))
    (test-case
      "A callback function should be called after a switch changes"
      (check-true (let* ((switch s-51-56)
                         (id (send switch get-id))
                         (old-pos (send switch get-position))
                         (success #f)
                         (f (lambda (fid new-pos)
                              (when (and (eq? id fid)
                                         (not (eq? old-pos new-pos)))
                                (set! success #t)))))
                    (send switch set-callback f)
                    (send switch set-position (if (= old-pos 1) 2 1))
                    success)))
    (test-case
      "When changing an inferior switch, its superior should change first"
      (check-true (let* ((inf-switch s-51-56)
                         (sup-switch s-5156-58)
                         (old-pos (send inf-switch get-position))
                         (success #f)
                         (f (lambda (fid new-pos)
                              (case fid
                                ((s-5156-58)
                                 (when (eq? success #f)
                                   (set! success 'tmp)))
                                ((s-51-56)
                                 (when (eq? success 'tmp)
                                   (set! success #t)))))))
                    (send sup-switch set-position 2)
                    (send inf-switch set-callback f)
                    (send sup-switch set-callback f)
                    (send inf-switch set-position (if (= old-pos 1) 2 1))
                    (and (= 1 (send sup-switch get-position))
                         success))))))



(define (run)
    (run-tests node-tests)
    (run-tests track-tests)
    (run-tests block-tests)
    (run-tests switch-tests))

