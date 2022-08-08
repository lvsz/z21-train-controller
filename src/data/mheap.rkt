#lang racket/base

(provide make-mheap
         mheap-empty?
         mheap-insert!
         mheap-delete!
         mheap-peek-at
         mheap-peek
         mheap-touch-at!)

(require racket/class
         data/gvector)

(struct mheap (data <<?))

(define (make-mheap <<?)
  (mheap (make-gvector) <<?))

(define (mheap-empty? mheap)
  (zero? (size mheap)))

(define (mheap-insert! mheap elem notify)
  (gvector-add! (mheap-data mheap) elem)
  (if (> (size mheap) 1)
    (sift-up! mheap (size mheap) notify)
    (notify 1 elem)))

(define (mheap-delete! mheap notify)
  (when (mheap-empty? mheap)
    (error "mheap-delete!: mheap empty" mheap))
  (let ((fst (mheap-peek mheap))
        (lst (gvector-remove-last! (mheap-data mheap))))
    (unless (mheap-empty? mheap)
      (store! mheap 1 lst notify)
      (sift-down! mheap 1 notify))
    fst))

(define (mheap-peek-at mheap idx)
  (when (mheap-empty? mheap)
    (error "mheap-peek-at: mheap empty" mheap))
  (gvector-ref (mheap-data mheap) (sub1 idx)))

(define (mheap-peek mheap)
  (when (mheap-empty? mheap)
    (error "mheap-peek: mheap empty" mheap))
  (mheap-peek-at mheap 1))

(define (mheap-touch-at! mheap idx notify)
  (cond ((= idx 1)
         (sift-down! mheap idx notify))
        ((= idx (size mheap))
         (sift-up! mheap idx notify))
        (((mheap-<<? mheap) (mheap-peek-at mheap (quotient idx 2))
                            (mheap-peek-at mheap idx))
         (sift-down! mheap idx notify))
        (else (sift-up! mheap idx notify))))

(define (size mheap)
  (gvector-count (mheap-data mheap)))

(define (store! mheap idx elem notify)
  (gvector-set! (mheap-data mheap) (sub1 idx) elem)
  (notify idx elem))

(define (sift-up! mheap idx notify)
  (let sift-iter ((child idx) (elem (mheap-peek-at mheap idx)))
    (let ((parent (quotient child 2)))
      (if (or (zero? parent)
              (not ((mheap-<<? mheap) elem (mheap-peek-at mheap parent))))
        (store! mheap child elem notify)
        (begin (store! mheap child (mheap-peek-at mheap parent) notify)
               (sift-iter parent elem))))))

(define (sift-down! mheap idx notify)
  (let ((<<? (mheap-<<? mheap)))
    (let sift-iter ((parent idx) (elem (mheap-peek-at mheap idx)))
      (let* ((left (* 2 parent))
             (right (add1 left))
             (smallest
               (cond ((< left (size mheap))
                      (if (<<? (mheap-peek-at mheap left)
                               (mheap-peek-at mheap right))
                        (if (<<? elem (mheap-peek-at mheap left))
                          parent
                          left)
                        (if (<<? elem (mheap-peek-at mheap right))
                          parent
                          right)))
                     ((or (> left (size mheap))
                          (<<? elem (mheap-peek-at mheap left)))
                      parent)
                     (else left))))
        (if (= smallest parent)
          (store! mheap parent elem notify)
          (begin (store! mheap parent (mheap-peek-at mheap smallest) notify)
                 (sift-iter smallest elem)))))))
