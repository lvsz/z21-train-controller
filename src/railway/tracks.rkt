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
         racket/function
         racket/list
         racket/match
         racket/set)

(define switch-tracks (make-hash))
(define switching-order (make-hash))

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
          ((list)    #f)
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
    (inherit-field id node-1 node-2 length)
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


;; I have basic switch S1 with 2 positions
;; I make 3-way switch S2-1, using basic switch as second position
;; I want to be able to set S2-1 to positions 1, 2 and 3
;; to know positions 2 and 3 means changing position-2 or S1,
;;      I have to go through the positions of any subswitches on creation
;;      and associate each with the correct number
;;      count number of possible positions, create vector
;;      store set-position lambdas in that vector
;;; for infrabel, let set-switch-position return a list of the form:
;;;   ((s3 . 2) (s2 . 2) (s1 . 1)), where pos-1 of s1 is s2, and pos-2 of s2 is s3
;;;   internally equal to (send s1 set-position 5)

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

    ; Removes tracks from their nodes, so that each node
    ; only lists up to two connected tracks
    (update-nodes! position-1 this)
    (update-nodes! position-2 this)

    ; After removing tracks from their nodes, store their data elsewhere
    (hash-set! switch-tracks position-1 this)
    (hash-set! switch-tracks position-2 this)

    (define position 1)

    (define/public (get-position)
      position)

    (define/public (set-position pos)
      (let ((track (case pos
                     ((1) position-1)
                     ((2) position-2)
                     (else (error "set-position: Invalid switch position")))))
        (map (lambda (switch!)
               (switch!))
             (hash-ref switching-order track))))

    (define (current)
      (if (= position 1)
        position-1
        position-2))

    (define (update! track pos)
      (hash-update!
        switching-order
        track
        (curry cons (lambda () (set! position pos)))
        (list (lambda () (set! position pos))))
      track)

    (define tracks
      (append (map (curryr update! 1)
                   (if (switch? position-1)
                     (send position-1 get-tracks)
                     (list position-1)))
              (map (curryr update! 2)
                   (if (switch? position-2)
                     (send position-2 get-tracks)
                     (list position-2)))))

    (define/public (get-tracks)
      tracks)

    (define nodes (get-nodes this))

    (define/override (get-connected-tracks)
      (let ((tracks (for/list ((n (in-list nodes)))
                      (send n get-tracks))))
        (remq this (apply set-union tracks))))

    ;; Node shared by the switch's components
    (define hinge-node
      (do ((n nodes (cdr n)))
        ((and (memq (car n) (get-nodes position-1))
              (memq (car n) (get-nodes position-2)))
         ;(begin (displayln (send (car n) get-id)) (car n)))))
         (car n))))

    ;(define (_from track full-info)
    ;  (let* ((from-nodes (get-nodes track))
    ;         (common-node (set-intersect from-nodes nodes)))
    ;    (cond ((or (null? common-node)              ; not connected
    ;               (not (null? (cdr common-node)))) ; same track?
    ;           #f)
    ;          ((eq? hinge-node (car common-node))
    ;           (if full-info
    ;             (let ((
    ;           ; coming from hinge-node, next depends on switch position
    ;           (let ((to-node (car (remq hinge-node (get-nodes (current))))))
    ;             (car (remq this (send to-node get-tracks)))))
    ;          (else
    ;           ; coming from any other node, next goes through hinge-node
    ;           (send hinge-node from this)))))

    (define/override (from track)
      (send (current) from track))

    ;; Return options, their positions and lengths
    (define/public (froms track)
      (from track))

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
    (set-union (get-nodes (get-field position-1 track))
               (get-nodes (get-field position-2 track)))
    (list (get-field node-1 track)
          (get-field node-2 track))))


; Used by switch% to clear node%s of its subtracks
(define (update-nodes! old-track new-track)
  (let ((nodes (get-nodes old-track)))
    (for ((node (in-list nodes)))
      (send node remove-track old-track)
      (send node add-track new-track))))

(define (top-track track)
  (hash-ref switch-tracks track track))
