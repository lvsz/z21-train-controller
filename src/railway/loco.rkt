#lang racket/base

(provide loco%)

(require racket/class
         "tracks.rkt")

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
      (set! on-d-block? #f))

    ;; Custom printing functions to make debugging easier
    (define/public (custom-write port)
      (fprintf port
               "(object:loco:~a:[~a->~a] ...)"
               id
               previous-track
               current-track))
    (define/public (custom-display port)
      (fprintf port "loco:~a" id))))
