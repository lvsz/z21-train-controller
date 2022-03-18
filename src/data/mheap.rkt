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

(struct mheap (data <<? (notify #:mutable)))

(define (make-mheap <<?)
  (mheap (make-gvector) <<? void))

(define (mheap-empty? mheap)
  (zero? (size mheap)))

(define (mheap-insert! mheap item proc)
  (set-mheap-notify! mheap proc)
  (gvector-add! (mheap-data mheap) item)
  (if (> (size mheap) 1)
    (sift-up mheap (size mheap))
    (proc 1 item)))

(define (mheap-delete! mheap proc)
  (when (mheap-empty? mheap)
    (error "mheap-delete!: mheap empty" mheap))
  (set-mheap-notify! mheap proc)
  (let* ((fst (get mheap 1))
         (lst (gvector-remove-last! (mheap-data mheap))))
    (if (mheap-empty? mheap)
      (proc 1 lst)
      (begin (store! mheap 1 lst)
             (sift-down mheap 1)))
    fst))

(define (mheap-peek-at mheap i)
  (when (mheap-empty? mheap)
    (error "mheap-peek-at: mheap empty" mheap))
  (get mheap i))

(define (mheap-peek mheap)
  (when (mheap-empty? mheap)
    (error "mheap-peek: mheap empty" mheap))
  (get mheap 1))

(define (mheap-touch-at! mheap i proc)
  (set-mheap-notify! mheap proc)
  (let ((parent (quotient i 2)))
    (cond ((= i 1)
           (sift-down mheap i))
          ((= i (size mheap))
           (sift-up mheap i))
          (((mheap-<<? mheap) (get mheap parent) (get mheap  i))
           (sift-down mheap i))
          (else (sift-up mheap i)))))


(define (size mheap)
  (gvector-count (mheap-data mheap)))

(define (get mheap i)
  (gvector-ref (mheap-data mheap) (sub1 i)))

(define (store! mheap i elem)
  (gvector-set! (mheap-data mheap) (sub1 i) elem))

(define (sift-up mheap i)
  (let sift-iter ((child i) (elem (get mheap i)))
    (let ((parent (quotient child 2)))
      (cond ((zero? parent)
             (store! mheap child elem))
            (((mheap-<<? mheap) elem (get mheap parent))
             (store! mheap child (get mheap parent))
             (sift-iter parent elem))
            (else (store! mheap child elem))))))

(define (sift-down mheap i)
  (let ((<<? (mheap-<<? mheap)))
    (let sift-iter ((parent i) (elem (get mheap i)))
      (let* ((left (* 2 parent))
             (right (add1 left))
             (smallest (cond ((< left (size mheap))
                              (if (<<? (get mheap left) (get mheap right))
                                (if (<<? elem (get mheap left))
                                  parent
                                  left)
                                (if (<<? elem (get mheap right))
                                  parent
                                  right)))
                             ((= left (size mheap))
                              (if (<<? elem (get mheap left))
                                parent
                                left))
                             (else parent))))
        (if (= smallest parent)
          (store! mheap parent elem)
          (begin (store! mheap parent (get mheap smallest))
                 (sift-iter smallest elem)))))))

#|
(define mheap%
  (class* object% (writable<%>)
    (init-field <<?)
    (field (storage (make-gvector)))
    (super-new)

    (define notify void)

    (define (get i)
      (gvector-ref storage (sub1 i)))

    (define (store! i a)
      (gvector-set! storage (sub1 i) a)
      (notify i a))

    (define (sift-up i)
      (let sift-iter ((child i) (element (get i)))
        (let ((parent (quotient child 2)))
          (cond ((zero? parent)
                 (store! child element))
                ((<<? element (get parent))
                 (store! child (get parent))
                 (sift-iter parent element))
                (else (store! child element))))))

    (define (sift-down i)
      (let ((size (gvector-count storage)))
        (let sift-iter ((parent i) (element (get i)))
          (let* ((left (* 2 parent))
                 (right (add1 left))
                 (smallest (cond ((< left size)
                                  (if (<<? (get left) (get right))
                                    (if (<<? element (get left))
                                      parent
                                      left)
                                    (if (<<? element (get right))
                                      parent
                                      right)))
                                 ((= left size)
                                  (if (<<? element (get left))
                                    parent
                                    left))
                                 (else parent))))
            (if (= smallest parent)
              (store! parent element)
              (begin (store! parent (get smallest))
                     (sift-iter smallest element)))))))

    (define/public (empty?)
      (zero? (gvector-count storage)))

    (define/public (insert! item proc)
      (set! notify proc)
      (gvector-add! storage item)
      (let ((size (gvector-count storage)))
        (if (> size 1)
          (sift-up size)
          (notify 1 item))))

    (define/public (delete! proc)
      (when (empty?)
        (error "Heap empty" this))
      (set! notify proc)
      (let* ((fst (get 1))
             (lst (gvector-remove-last! storage)))
        (if (empty?)
          (notify 1 lst)
          (begin (store! 1 lst)
                 (sift-down 1)))
        fst))

    (define/public (peek-at i)
      (when (empty?)
        (error "Heap empty" this))
      (get i))

    (define/public (peek)
      (peek-at 1))

    (define/public (touch-at! i proc)
      (set! notify proc)
      (let ((parent (quotient i 2))
            (size (gvector-count storage)))
        (cond ((= i 1)
               (sift-down i))
              ((= i size)
               (sift-up i))
              ((<<? (get parent) (get i))
               (sift-down i))
              (else (sift-up i)))))

    (define/public (custom-write port)
      (write "#<mheap>" port))
    (define/public (custom-display port)
      (custom-write port))))
|#

