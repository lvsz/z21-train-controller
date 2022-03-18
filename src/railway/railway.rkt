#lang racket/base


(provide railway%
         track?
         d-block?
         switch?
         setup-ids)

(require racket/class
         racket/list
         "loco.rkt"
         "tracks.rkt"
         "setup.rkt"
         "../data/pqueue.rkt")


;; An instance of the railway% class is can load a chosen railway setup
;; and maintain its state. Both nmbs% and infrabel% use this class
;; to keep track of the railway. The setup-id field takes one of the
;; files names found in resources/setups as a symbol.
(define railway%
  (class object%
    (init-field setup-id)
    (super-new)

    (define setup (get-setup setup-id))

    (define nodes    (make-hash))
    (define tracks   (make-hash))
    (define d-blocks (make-hash))
    (define switches (make-hash))
    (define locos    (make-hash))

    (define/public (get-track id)
      (hash-ref tracks id))
    (define/public (get-tracks)
      (hash-values tracks))

    (define/public (get-d-block id)
      (hash-ref d-blocks id))
    (define/public (get-d-blocks)
      (hash-values d-blocks))

    (define/public (get-switch id)
      (hash-ref switches id))
    (define/public (get-switches)
      (hash-values switches))

    (define/public (get-loco id)
      (hash-ref locos id))
    (define/public (get-locos)
      (hash-values locos))

    (define/public (add-loco id prev-track curr-track)
      (hash-set! locos id (make-object loco% id prev-track curr-track)))
    (define/public (remove-loco id)
      (hash-remove! locos id))

      (construct setup nodes tracks d-blocks switches locos)))

;; Using a given setup, this function reads the file, evaluating it line by
;; line, creating objects and storing them in the given hash-maps.
(define (construct setup nodes tracks d-blocks switches locos)
  (define (new-node id)
    (if (hash-has-key? nodes id)
      (hash-ref nodes id)
      (let ((node (make-object node% id)))
        (hash-set! nodes id node)
        node)))
  (define (new-track id n1 n2 length)
    (hash-set! tracks id (make-object track% id n1 n2 length)))
  (define (new-d-block id n1 n2 length)
    (let ((d-block (make-object d-block% id n1 n2 length)))
      (hash-set! tracks id d-block)
      (hash-set! d-blocks id d-block)))
  (define (new-switch id t1 t2)
    (let ((switch (make-object switch% id t1 t2)))
      (hash-set! tracks id switch)
      (hash-set! switches id switch)))
  (define (new-loco id track-1 track-2)
    (hash-set! locos id (make-object loco% id track-1 track-2)))

  (define read-id string->symbol)
  (define read-number string->number)
  (define (read-node s)
    (new-node (read-id s)))
  (define (read-track s)
    (hash-ref tracks (read-id s)))
  (define (add new-object procs params)
    (apply new-object (map (lambda (f x) (f x)) procs params)))
  (for ((input (in-list (read-setup setup))))
    (let ((type   (string->symbol (car input)))
          (params (cdr input)))
      (case type
        ((T) (add new-track
                  (list read-id read-node read-node read-number)
                  params))
        ((D) (add new-d-block
                  (list read-id read-node read-node read-number)
                  params))
        ((S) (add new-switch
                  (list read-id read-track read-track)
                  params))
        ((L) (add new-loco
                  (list read-id read-track read-track)
                  params))))))


#|

(define (dijkstra tracks start)
  (let ((dist (make-hash (list (cons start 0))))
        (prev (make-hash))
        (pq (make-pqueue <))
        (pq-ids (make-hash (cons start 0))))

    (define (notify pq-id track distance)
      (hash-set! pq-ids track pq-id))

    (define (relax!)
      )

    (pqueue-enqueue! pq start 0 notify)
    (for ((t (in-list (remq start tracks))))
      (hash-set! dist t +inf.0)
      (hash-set! prev t #f)
      (hash-set!)
      (pqueue-enqueue! pq t +inf.0 notify))

    (let loop ()
      (let-values (((from distance) (serve! pq notify)))
        (for 

(define (dijkstra start tracks (avoid '()))
  (let ((distances (make-hash (map (lambda (t) (cons t +inf.0)) tracks)))
        (how-to-reach (make-hash (map (lambda (t) (cons t '())) tracks)))
        (pq-ids (make-hash (map (lambda (t) (cons t '()) tracks))))
        (pq (priority-que <)))
    (define (notify pq-id track distance)
      (hash-set! pq-ids track pq-id))
    (define (pq-id-of track)
      (hash-ref pq-ids track))
    (define (relax! from to)
      (let* ((weight (send to get-length))
             (new-distance (+ (hash-ref distances from) weight)))
        (when (< new-distance (hash-ref distances to))
          (hash-set! distances to new-distance)
          (hash-set! how-to-reach to from)
          (unless (queue-empty? pq

                                |#
