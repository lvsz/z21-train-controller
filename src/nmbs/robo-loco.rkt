#lang racket/base

(provide robo-loco%)

(require racket/class
         racket/match
         racket/vector
         "../railway/railway.rkt")

;;; A class that can control a locomotive, going along a given route
(define robo-loco%
  (class object%
    (init-field nmbs loco ((route-list route)) (on-finish void))
    (super-new)

    (define speed 100)
    (define sleep-time 0.5)

    (define loco-id (send loco get-id))
    (define route (list->vector route-list))
    (define route-length (vector-length route))

    (define (curr i)
      (vector-ref route i))
    (define (next i)
      (vector-ref route (add1 i)))
    (define (prev i)
      (vector-ref route (sub1 i)))

    ;; Change direction when route's next track is loco's prev track
    (when (eq? (top-track (vector-ref route 1))
               (top-track (get-field previous-track loco)))
      (send nmbs change-loco-direction loco-id))

    ;; Put all switches before the next d-block in the correct position
    (define (set-switches pos)
      (let* ((track (vector-ref route pos))
             (top (top-track track)))
        (if (d-block? track)
          (choo-choo pos)
          (begin (when (switch? top)
                   (let ((top-id (send top get-id))
                         (track-id (send track get-id)))
                     (send nmbs set-switch-track top-id track-id)))
                 (set-switches (add1 pos))))))

    ;; Check on which d-blocks if any the loco has to change direction
    (define reversals
      (for/list ((i (in-range 3 (sub1 route-length)))
                 #:when (eq? (top-track (vector-ref route (- i 2)))
                             (top-track (vector-ref route i))))
        (vector-ref route (sub1 i))))

    ;; Get nmbs to update the loco's location
    (define (update-loco curr-id (prev-id #f))
      (send nmbs
            update-loco-location
            loco-id
            curr-id
            prev-id))

    ;; Loop runs until loco reaches destination or gets lost
    (define (choo-choo pos)
      (let ((lt (send loco get-location))
            (db (send nmbs get-loco-d-block loco-id)))
        (cond
          ; opt 1: we're finished
          ((and (>= pos (sub1 route-length))
                (eq? lt (vector-ref route (sub1 route-length))))
           (update-loco (send lt get-id)
                        (send (vector-ref route (- route-length 2))
                              get-id))
           #t)
          ; opt 2: just passed a d-block, time to switch switches
          ((not (d-block? (curr pos)))
           ; returns thru mutual recursion
           (set-switches pos))
          ; opt 3: loco is on a d-block
          (db
            (if (eq? db (send lt get-id))
              ; opt 3a: track given by nmbs matches loco's last known location
              (begin
                (when (and (pair? reversals)
                           (eq? lt (car reversals)))
                  (sleep sleep-time)
                  (set! reversals (cdr reversals))
                  (send nmbs change-loco-direction loco-id))
                (let wait ((db (send nmbs get-loco-d-block loco-id)))
                  ; wait in a loop until loco shows up on a d-block
                  (sleep sleep-time)
                  (cond ((not db)
                         ; not on a d-block yes, wait longer
                         (wait (send nmbs get-loco-d-block loco-id)))
                        ((eq? db (send (curr pos) get-id))
                         ; good d-block found; continue
                         (update-loco (send (curr pos) get-id)
                                      (send (prev pos) get-id))
                         (choo-choo (add1 pos)))
                        ((not (memq db (map (lambda (x) (send x get-id))
                                            route-list)))
                         ; check if loco is still on planned route
                         (update-loco db)
                         #f)
                        (else
                          (wait (send nmbs get-loco-d-block loco-id))))))
              ; opt 3b: no match, update location and check if still on route
              (let ((id-vec (vector-map (lambda (x) (send x get-id)) route)))
                (update-loco db)
                ; check loco is still on the planned route
                (match (vector-memq db id-vec)
                  (#f #f)
                  (n  (choo-choo (add1 n)))))))
          ; opt 4: wait & see
          (else
            (sleep sleep-time)
            (choo-choo pos)))))

    (define/public (start)
      (thread (lambda ()
                (send nmbs set-loco-speed loco-id speed)
                (begin0 (choo-choo 1)
                        (send nmbs set-loco-speed loco-id 0)
                        (on-finish)))))))

