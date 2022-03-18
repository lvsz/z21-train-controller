#lang racket/base

(provide node%
         track%
         d-block%
         switch%
         track?
         d-block?
         switch?)

(require racket/class
         racket/function
         racket/list
         racket/match
         racket/set)

(define (track? track)
  (is-a? track track%))

(define (d-block? track)
  (is-a? track d-block%))

(define (switch? track)
  (is-a? track switch%))

;; Stores the connection between 2 tracks,
;; or signifies the end of one,
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
      (set! tracks (remq track tracks)))

    (define/public (from track)
      (let ((that-track (top-track track)))
        (if (or (not (memq that-track tracks)) ; no connection
                (null? (cdr tracks)))          ; dead end
          #f
          (car (remq that-track tracks)))))))

;; Most basic piece of a railway, consists of 2 nodes and a length,
;; length is used to calculate routes.
(define track%
  (class object%
    (init-field id node-1 node-2 length)
    (super-new)
    (field (superior this))

    (send node-1 add-track this)
    (send node-2 add-track this)

    (define/public (get-id)
      id)

    (define/public (get-length)
      length)

    (define/public (get-connected-tracks)
      (let ((this-track (top-track this)))
        (append (remq this-track (send node-1 get-tracks))
                (remq this-track (send node-2 get-tracks)))))

    (define/public (from track)
      (let ((that-track (top-track track)))
        (match (remq that-track (get-connected-tracks))
          ((list to) to)
          (_         #f))))))


;; A detection block is able to tell whether a train is located on it,
;; or on a neighbouring detection block.
(define d-block%
  (class track%
    (init ((_id     id))
          ((_node-1 node-1))
          ((_node-2 node-2))
          ((_length length)))
    (super-make-object _id _node-1 _node-2 _length)
    (inherit-field id node-1 node-2 length superior)
    (field (status 'green))
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


;; A switch is a compound track, it has 2 positions it can switch between,
;; it is possible for one or both of those positions to be switches as well
(define switch%
  (class track%
    (init ((_id id)))
    (init-field track-1 track-2)
    (super-make-object _id
                       (get-field node-1 track-1)
                       (get-field node-2 track-1)
                       (get-field length track-1))
    (inherit-field id superior)
    (field (base-tracks (append (if (switch? track-1)
                                  (get-field base-tracks track-1)
                                  (list track-1))
                                (if (switch? track-2)
                                  (get-field base-tracks track-2)
                                  (list track-2)))))
    (set! superior this)
    (set-field! superior track-1 this)
    (set-field! superior track-2 this)

    ; Removes tracks from their nodes, so that each node
    ; only lists up to two connected tracks
    (update-nodes! track-1 this)
    (update-nodes! track-2 this)

    (define current-position track-1)

    (define/public (get-position)
      (if (eq? current-position track-1)
        1
        2))

    (define/public (set-position pos)
      (let ((t (cond ((or (eq? pos 1) (eq? pos track-1)) track-1)
                     ((or (eq? pos 2) (eq? pos track-2)) track-2)
                     (else (error "set-position: Invalid switch position" pos)))))
        (unless (eq? superior this)
          (send superior set-position this))
        (set! current-position t)
        (callback id (get-position))))

    (define nodes (get-nodes this))

    (define/override (get-connected-tracks)
      (let ((tracks (for/list ((n (in-list nodes)))
                      (send n get-tracks))))
        (remq (top-track this) (apply set-union tracks))))

    ;; Node shared by the switch's components
    (define hinge-node
      (do ((n nodes (cdr n)))
        ((and (memq (car n) (get-nodes track-1))
              (memq (car n) (get-nodes track-2)))
         (car n))))

    (define/override (from track)
      (send current-position from track))

    ;; Returns a list of exit options when coming from a track
    (define/public (options-from track)
      (for/list ((to (in-list
                       (map (lambda (t)
                              (send t from track))
                            base-tracks)))
                 #:when to)
        to))

    (define/override (get-length)
      (send current-position get-length))

    ; A function that get called every time the switch changes position
    (define callback void)
    (define/public (set-callback proc)
      (set! callback proc))))


; Helper function that gets all nodes related to a track,
; mostly useful when dealing with switches
(define (get-nodes track)
  (if (switch? track)
    (set-union (get-nodes (get-field track-1 track))
               (get-nodes (get-field track-2 track)))
    (list (get-field node-1 track)
          (get-field node-2 track))))


; Used by switch% to clear node%s of its subtracks
(define (update-nodes! old-track new-track)
  (let ((nodes (get-nodes old-track)))
    (for ((node (in-list nodes)))
      (send node remove-track old-track)
      (send node add-track new-track))))

(define (top-track track)
  (let ((st (get-field superior track)))
    (if (eq? st track)
      track
      (top-track st))))
