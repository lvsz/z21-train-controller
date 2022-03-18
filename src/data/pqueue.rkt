#lang racket/base

(provide make-pqueue
         pqueue-empty?
         pqueue-enqueue!
         pqueue-serve!
         pqueue-reschedule!
         pqueue-peek
         pqueue-priority-of)

(require "mheap.rkt")

(struct pqueue (mheap))

(struct item (value (priority #:mutable)) #:transparent)

(define (pair->item x)
  (item (car x) (cdr x)))

(define (on accessor cmp)
  (lambda (x y)
    (cmp (accessor x)
         (accessor y))))

(define (pqueue-notify notify)
  (lambda (i item)
    (notify i (item-value item) (item-priority item))))

(define (make-pqueue <<?)
  (pqueue (make-mheap (on item-priority <<?))))

(define (pqueue-empty? pq)
  (mheap-empty? (pqueue-mheap pq)))

(define (pqueue-enqueue! pq val priority notify)
  (mheap-insert! (pqueue-mheap pq) (item val priority) (pqueue-notify notify)))

(define (pqueue-serve! pq notify)
  (when (pqueue-empty? pq)
    (error "pqueue-serve!: empty pqueue" pq))
  (let ((pq-item (mheap-delete! (pqueue-mheap pq) (pqueue-notify notify))))
    (values (item-value pq-item) (item-priority pq-item))))

(define (pqueue-reschedule! pq i new-priority notify)
  (let ((pq-item (mheap-peek-at (pqueue-mheap pq) i)))
    (set-item-priority! pq-item new-priority)
    (mheap-touch-at! (pqueue-mheap pq) i (pqueue-notify notify))))

(define (pqueue-peek pq)
  (when (pqueue-empty? pq)
    (error "pqueue-peek: empty pqueue" pq))
  (item-value (mheap-peek (pqueue-mheap pq))))

(define (pqueue-peek-at pq i)
  (when (pqueue-empty? pq)
    (error "pqueue-peek-at: empty pqueue" pq))
  (item-value (mheap-peek-at (pqueue-mheap pq) i)))

(define (pqueue-priority-of pq i)
  (when (pqueue-empty? pq)
    (error "pqueue-peek-at: empty pqueue" pq))
  (item-priority (mheap-peek-at (pqueue-mheap pq) i)))

