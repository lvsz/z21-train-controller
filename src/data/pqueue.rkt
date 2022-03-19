#lang racket/base

(provide make-pqueue
         pqueue-empty?
         pqueue-enqueue!
         pqueue-serve!
         pqueue-reschedule!
         pqueue-peek
         pqueue-priority-of)

(require "mheap.rkt")

(struct pqueue (mheap indices))

(struct item (value (priority #:mutable)) #:transparent)

;(define (pq-notify notify)
;  (lambda (idx item)
;    (notify idx (item-value item) (item-priority item))))

(define (pq-notify pq)
  (lambda (idx val)
    (hash-set! (pqueue-indices pq) (item-value val) idx)))

(define (make-pqueue compare)
  (pqueue (make-mheap (lambda (x y)
                        (compare (item-priority x) (item-priority y))))
          (make-hash)))

(define (pqueue-empty? pq)
  (mheap-empty? (pqueue-mheap pq)))

(define (pqueue-enqueue! pq val priority)
  (mheap-insert! (pqueue-mheap pq) (item val priority) (pq-notify pq)))

(define (pqueue-serve! pq)
  (when (pqueue-empty? pq)
    (error "pqueue-serve!: empty pqueue" pq))
  (let ((pq-item (mheap-delete! (pqueue-mheap pq) (pq-notify pq))))
    (hash-remove! (pqueue-indices pq) (item-value pq-item))
    (values (item-value pq-item) (item-priority pq-item))))

(define (pqueue-reschedule! pq val new-priority)
  (let* ((idx (hash-ref (pqueue-indices pq) val))
         (pq-item (mheap-peek-at (pqueue-mheap pq) idx)))
    (set-item-priority! pq-item new-priority)
    (mheap-touch-at! (pqueue-mheap pq) idx (pq-notify pq))))

(define (pqueue-peek pq)
  (when (pqueue-empty? pq)
    (error "pqueue-peek: empty pqueue" pq))
  (item-value (mheap-peek (pqueue-mheap pq))))

(define (pqueue-peek-at pq idx)
  (when (pqueue-empty? pq)
    (error "pqueue-peek-at: empty pqueue" pq))
  (item-value (mheap-peek-at (pqueue-mheap pq) idx)))

(define (pqueue-priority-of pq val)
  (when (pqueue-empty? pq)
    (error "pqueue-peek-at: empty pqueue" pq))
  (item-priority (mheap-peek-at (pqueue-mheap pq)
                                (hash-ref (pqueue-indices pq) val))))

