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
      (set! tracks (remq track tracks)))))

;; Most basic piece of a railway, consists of 2 nodes and a length,
;; length is used to calculate routes.
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

    ; TODO: make it work for tracks that are part
    ;       of a switch
    (define/public (from track)
      (match (remq track (get-connected-tracks))
        ((list)    #f)
        ((list to) to)
        (_         #f)))))


;; A detection block is able to tell whether a train is located on it,
;; or on a neighbouring detection block.
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


;; A switch is a compound track, it has 2 position it can switch between,
;; it is possible for one or both of those positions to be switches as well
(define switch%
  (class track%
    (init ((_id id)))
    (init-field position-1 position-2)
    (super-make-object _id
                       (get-field node-1 position-1)
                       (get-field node-2 position-1)
                       (get-field length position-1))
    (inherit-field id)
    (field (_set-position (lambda (pos)
                            (set! position pos)
                            (callback))))

    ;; If this switch contains one or more switches, its position needs to
    ;; align with any of its sub-switches whenever their position gets changed
    (define (automate-3+way track super-pos)
      (let ((old-proc (get-field _set-position track)))
        (set-field! _set-position
                    track
                    (lambda (sub-pos)
                      (_set-position super-pos)
                      (old-proc sub-pos)))))
    (when (switch? position-1)
      (automate-3+way position-1 1))
    (when (switch? position-2)
      (automate-3+way position-2 2))

    ; Removes tracks from their nodes, so that each node
    ; only lists up to two connected tracks
    (update-nodes! position-1 this)
    (update-nodes! position-2 this)

    (define position 1)

    (define/public (get-position)
      position)

    (define/public (set-position pos)
      (_set-position pos))

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

    (define/override (from track)
      (send (current) from track))

    (define/override (get-length)
      (send (current) get-length))

    ; A function that get called every time the switch changes position
    ;(define callback-proc void)
    (define callback void)
    ;(define (callback) (callback-proc))
    (define/public (set-callback proc)
      ;(set! callback-proc proc))))
      (set! callback proc))))


;; A locomotive, need two tracks to places on a railway in order
;; to determine its initial direction
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


; Helper function that gets all nodes related to a track,
; mostly useful when dealing with switches
(define (get-nodes track)
  (if (switch? track)
    (append (get-nodes (get-field position-1 track))
            (get-nodes (get-field position-2 track)))
    (list (get-field node-1 track)
          (get-field node-2 track))))


; Used by switch% to clear node%s of its subtracks
(define (update-nodes! old-track new-track)
  (let ((nodes (get-nodes old-track)))
    (for ((node (in-list nodes)))
      (send node remove-track old-track)
      (send node add-track new-track))))

