#lang racket/base

(provide node%
         track%
         d-block%
         switch%
         loco%
         track?
         d-block?
         switch?)

(require racket/class
         racket/list
         racket/match)


(define (track? track)
  (is-a? track track%))

(define (d-block? track)
  (is-a? track d-block%))

(define (switch? track)
  (is-a? track switch%))

(define node%
  (class object%
    (init-field id)
    (super-new)

    (define tracks '())

    (define/public (get-id)
      id)

    (define/public (get-tracks)
      tracks)

    (define/public (add-track track)
      (unless (memq track tracks)
        (set! tracks (cons track tracks))))

    (define/public (remove-track track)
      (set! tracks (remq track tracks)))))

(define track%
  (class object%
    (init-field id node-1 node-2 length)
    (super-new)
    (send node-1 add-track this)
    (send node-2 add-track this)

    (define/public (get-id)
      id)

    (define/public (get-length)
      length)

    (define/public (get-connected-tracks)
      (append (remq this (send node-1 get-tracks))
              (remq this (send node-2 get-tracks))))

    (define (has-node? track node)
      (or (eq? (get-field node-1 track) node)
          (eq? (get-field node-2 track) node)))

    (define/public (from track)
      (match (remq track (get-connected-tracks))
        ((list)    #f)
        ((list to) to)
        (_ (error "not connected"))))))

(define d-block%
  (class track%
    (init ((_id id))
          ((_node-1 node-1))
          ((_node-2 node-2))
          ((_length length)))
    (field (status 'green))
    (super-make-object _id _node-1 _node-2 _length)
    (inherit-field id node-1 node-2 length)
    (inherit get-connected-tracks)

    (define connected-blocks
      (for/list ((track (in-list (get-connected-tracks)))
                 #:when (d-block? track))
        (send track connect-block this)
        track))

    (define/public (connect-block d-block)
      (set! connected-blocks (cons d-block connected-blocks)))

    (define/public (get-status)
      status)

    (define/public (occupy)
      (set! status 'red)
      (for ((d-block (in-list connected-blocks))
            #:when (eq? (get-field status d-block) 'green))
        (set-field! status d-block 'orange)))

    (define/public (clear)
      (if (for/or ((d-block (in-list connected-blocks)))
            (eq? (get-field status d-block) 'red))
        (set! status 'orange)
        (begin (set! status 'green)
               (for ((d-block (in-list connected-blocks))
                     #:unless (eq? (get-field status d-block) 'green))
                 (send d-block clear)))))))

(define (update-nodes! old-track new-track)
  (if (switch? old-track)
    (begin (update-nodes! (get-field position-1 old-track) new-track)
           (update-nodes! (get-field position-2 old-track) new-track))
    (let ((node-1 (get-field node-1 old-track))
          (node-2 (get-field node-2 old-track)))
      (send node-1 remove-track old-track)
      (send node-1 add-track    new-track)
      (send node-2 remove-track old-track)
      (send node-2 add-track    new-track))))

(define switch%
  (class track%
    (init ((_id id)))
    (init-field position-1 position-2)
    (super-make-object _id
                       (get-field node-1 position-1)
                       (get-field node-2 position-1)
                       (get-field length position-1))
    (inherit-field id)

    (update-nodes! position-1 this)
    (update-nodes! position-2 this)

    (define position 1)

    (define/public (get-position)
      position)

    (define/public (set-position pos)
      (set! position pos)
      (callback))

    (define (current)
      (if (= position 1)
        position-1
        position-2))

    (define/public (get-track)
      (if (switch? (current))
        (send (current) get-position)
        (current)))

    (define tracks
      (append (if (switch? position-1)
                (send position-1 get-tracks)
                (list position-1))
              (if (switch? position-2)
                (send position-2 get-track)
                (list position-2))))

    (define/public (get-tracks)
      tracks)

   ; (define/public (set-track track)
   ;   (cond ((eq? track position-1)
   ;          (set! position position-1))
   ;         ((eq? track position-2)
   ;          (set! position position-2))
   ;         ((and (switch? position-1)
   ;               (memq track (send position-1 get-positions)))
   ;          (set! position position-1)
   ;          (send position-1 set-position! track))
   ;         ((and (switch? position-2)
   ;               (memq track (send position-2 get-positions)))
   ;          (set! position position-2)
   ;          (send position-2 set-position! track))))

    (define/override (from track)
      (send (current) from track))

    (define/override (get-length)
      (send (current) get-length))

    (define callback void)
    (define/public (set-callback proc)
      (set! callback proc))))

(define loco%
  (class object%
    (init-field id previous-track current-track)
    (field (speed 0)
           (direction 1))
    (super-new)

    (define/public (get-id)
      id)

    (define/public (get-location)
      current-track)

    (define on-d-block? #f)

    (define/public (update-location new-track (new-prev-track current-track))
      (set! on-d-block? (d-block? new-track))
      (set! current-track new-track)
      (set! previous-track new-prev-track))

    (define/public (get-speed)
      speed)

    (define/public (set-speed new-speed)
      (set! speed new-speed))

    (define/public (get-direction)
      direction)

    (define/public (change-direction)
      (set! previous-track (send current-track from previous-track))
      (set! direction (- direction)))

    (define/public (get-d-block)
      (if on-d-block?
        current-track
        #f))

    (define/public (left-d-block)
      (set! on-d-block? #f))))

