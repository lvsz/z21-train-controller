#lang racket/base

(provide railway%
         track?
         d-block?
         switch?
         top-track?
         top-track
         setup-ids
         get-id
         get-ids)

(require racket/class
         racket/list
         racket/set
         "loco.rkt"
         "tracks.rkt"
         "setup.rkt"
         "../data/pqueue.rkt"
         (for-syntax racket/base))


;; Helper syntax to retrieve an objects id
(define-syntax (get-id stx)
  (syntax-case stx ()
    ((_ obj) #'(send obj get-id))))

;; Helper function to turn a list of objects into a list of ids
(define (get-ids lst)
  (map (lambda (obj) (send obj get-id)) lst))


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

    ;; Parse the setup and fill the datastructures
    (construct setup
               nodes
               tracks
               d-blocks
               switches
               locos)

    (define prev+dist (make-hash))
    (define route+dist (make-hash))

    (define/public (get-route+distance from to)
      (unless (and (d-block? from) (d-block? to))
        (error "get-route+distance: Only routes between d-blocks available"))
      (define (unfold to p+d)
        (let loop ((curr to) (route '()))
          (let ((prev (hash-ref p+d curr #f)))
            (if prev
              (loop (car prev) (cons curr route))
              (cons curr route)))))
      (let ((p+d (hash-ref prev+dist from)))
        (if (hash-has-key? p+d to)
          (values (unfold to p+d) (cdr (hash-ref p+d to)))
          (let ((r+d (hash-ref route+dist from)))
            (if (hash-has-key? r+d to)
              (let* ((d-blocks (unfold to r+d))
                     (route (let rec ((head (car d-blocks))
                                      (tail (cdr d-blocks)))
                              (if (null? tail)
                                '(())
                                (append (unfold (car tail)
                                                (hash-ref prev+dist head))
                                        (cdr (rec (car tail) (cdr tail))))))))
                (values route (cdr (hash-ref r+d to))))
              (values #f #f))))))

    (define/public (get-route from to)
      (let-values (((route dist) (get-route+distance from to)))
        route))

    (let ((t-tracks (for/list
                      ((t (in-hash-values tracks))
                       #:when (top-track? t))
                      t)))
      (for ((d-block (in-hash-values d-blocks)))
        (hash-set! prev+dist
                   d-block
                   (dijkstra t-tracks d-block)))
      (for ((d-block (in-hash-values d-blocks)))
        (hash-set! route+dist
                   d-block
                   (dijkstra-2 prev+dist d-block))))))


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


;; Special version of Dijkstra which implicitly assumes all edges are directed
;; away from the source when they're first enountered. If the shortest path
;; between two detection blocks doesn't require reversing the train, this will
;; find it.
(define (dijkstra tracks start (avoid '()))
  (let ((dist (make-hash (list (cons start 0))))
        (prev (make-hash))
        (pq (make-pqueue <=)))

    (define (relax! from)
      (lambda (to)
        (let ((length-to (send to get-length))
              (dist-from (hash-ref dist from)))
          (when (< (+ dist-from length-to)
                   (hash-ref! dist to +inf.0))
            (hash-set! dist to (+ dist-from length-to))
            (hash-set! prev to from)
            (unless (pqueue-empty? pq)
              (pqueue-reschedule! pq to (+ dist-from length-to)))))))

    (define (for-each-option proc to #:from (from #f))
      (for-each proc (set-subtract
                       (if (switch? to)
                         (if from
                           (send to options-from from)
                           (get-field options to))
                         (list to))
                       avoid)))

    (for ((track (in-list (set-subtract tracks (cons start avoid)))))
      (for-each-option (lambda (opt)
                         (pqueue-enqueue! pq opt +inf.0))
                       track))
    (for ((track (in-list (send start get-connected-tracks))))
      (for-each-option (relax! start) track #:from start))

    (let find-path ()
      (let*-values (((track distance) (pqueue-serve! pq))
                    ((prev-track) (hash-ref prev track #f)))
        (when prev-track
          (let ((to (send track from prev-track)))
            (when to
              (for-each-option (relax! track) to #:from track)))
          (unless (pqueue-empty? pq)
            (find-path)))))

    (for/hash (((to from) (in-hash prev)))
      (values to (cons from (hash-ref dist to))))))


;; This version of the Dijkstra algorithm utlises the detection blocks as
;; vertices, while the routes generated between them by other Dijkstra
;; implementation serve as edges.
;; Simply put, if d-block A already has a route to B, and B to C, then this
;: will calculate the best route from A to C by making the train reverse on
;; on d-block B. If this maneuver was not needed for the shortest route, the
;; previous algorithm would have already found a route from A to C.
(define (dijkstra-2 rs+ds start)
  (let ((dist (make-hash (list (cons start 0))))
        (prev (make-hash))
        (pq (make-pqueue <)))

    (define (relax! from)
      (lambda (to)
        (let ((length-to (cdr (hash-ref (hash-ref rs+ds from) to)))
              (dist-from (hash-ref dist from)))
          (when (< (+ dist-from length-to)
                   (hash-ref! dist to +inf.0))
            (hash-set! dist to (+ dist-from length-to))
            (hash-set! prev to from)
            (unless (pqueue-empty? pq)
              (pqueue-reschedule! pq to (+ dist-from length-to)))))))

    (for ((track (in-hash-keys rs+ds))
          #:unless (eq? track start))
      (pqueue-enqueue! pq track +inf.0))
    (for ((track (in-hash-keys (hash-ref rs+ds start)))
          (i (in-naturals))
          #:when (d-block? track))
      ((relax! start) track))

    (let find-path ()
      (let*-values (((track distance) (pqueue-serve! pq))
                    ((prev-track) (hash-ref prev track #f)))
        (when prev-track
          (let ((next (filter d-block? (hash-keys (hash-ref rs+ds track)))))
            (for-each (relax! track) next))
          (unless (pqueue-empty? pq)
            (find-path)))))

    (for/hash (((to from) (in-hash prev)))
      (values to (cons from (hash-ref dist to))))))

