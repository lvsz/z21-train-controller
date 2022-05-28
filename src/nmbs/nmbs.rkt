#lang racket/base

(require racket/class
         racket/list
         "gui.rkt"
         "robo-loco.rkt"
         "../railway/railway.rkt"
         "../logger.rkt")

(provide nmbs%)

(define nmbs%
  (class object%
    (init-field infrabel (log-level #f))
    (super-new)

    (when log-level
      (start-logger log-level))

    (define railway #f)

    ; List of functions to be called when a switch changes
    (define switch-listeners '())
    (define/public (add-switch-listener fn)
      (set! switch-listeners (cons fn switch-listeners)))

    ; List of functions to be called when a detection block changes
    (define d-block-listeners '())
    (define/public (add-d-block-listener fn)
      (set! d-block-listeners
            (cons fn d-block-listeners)))

    ; Hashmap of lists of functions called when a loco changes speed
    (define loco-speed-listeners (make-hash))
    (define/public (add-loco-speed-listener loco-id fn)
      (if (hash-has-key? loco-speed-listeners loco-id)
        (hash-update! loco-speed-listeners loco-id (lambda (fns) (cons fn fns)))
        (hash-set! loco-speed-listeners loco-id (list fn))))


    (define (get-id obj)
      (send obj get-id))
    (define/public (get-loco-ids)
      (map get-id (send railway get-locos)))
    (define/public (get-switch-ids)
      (map get-id (send railway get-switches)))
    (define/public (get-d-block-ids)
      (map get-id (send railway get-d-blocks)))

    (define/public (get-switch-position id)
      (send (send railway get-switch id) get-position))
    (define/public (set-switch-position id pos)
      (send (send railway get-switch id) set-position pos))

    (define (get-loco id)
      (send railway get-loco id))
    (define/public (get-loco-speed id)
      (abs (send infrabel get-loco-speed id)))

    ; Change the speed, notifying any listeners
    (define/public (set-loco-speed id speed)
      (let ((speed (abs speed)))
        (for ((notify (in-list (hash-ref loco-speed-listeners id))))
          (notify speed))
        (send infrabel set-loco-speed id
              (* (send (get-loco id) get-direction) speed))))
    (define/public (change-loco-direction id)
      (send infrabel set-loco-speed id (- (send infrabel get-loco-speed id)))
      (send (get-loco id) change-direction))

    ; Nmbs generates a list of viable starting spots
    ; to add new locomotives
    (define/public (add-loco spot-id)
      (let* ((spot (hash-ref starting-spots spot-id))
             (curr-id (starting-spot-current spot))
             (prev-id (starting-spot-previous spot))
             (loco-id (gensym "L")))
        (send infrabel add-loco loco-id prev-id curr-id)
        (send railway add-loco
              loco-id
              (send railway get-track prev-id)
              (send railway get-track curr-id))
        loco-id))

    (define/public (remove-loco loco-id)
      (send infrabel remove-loco loco-id)
      (send railway remove-loco loco-id)
      (thread-suspend update-thread)
      (hash-remove! loco-speed-listeners loco-id)
      (thread-resume update-thread))

    (define/public (get-loco-d-block id)
      (send infrabel get-loco-d-block id))

    (define/public (update-loco-location loco-id curr-id (prev-id #f))
      (if prev-id
        (send (get-loco loco-id)
              update-location
              (send railway get-track curr-id)
              (send railway get-track prev-id))
        (send (get-loco loco-id)
              update-location
              (send railway get-track curr-id))))

    ; Get hash of spots where a new loco can be added,
    ; to be a valid spot, a detection block is needed whose local id and
    ; that of a connected segment match those imported through infrabel.
    (define starting-spots #f)
    (define/public (get-starting-spots)
      (if starting-spots
        (hash-keys starting-spots)
        '()))

    (define/public (route loco-id dest)
      (let* ((loco (get-loco loco-id))
             (route (send railway
                          get-route
                          (send loco get-location)
                          (send railway get-d-block dest)))
             (robo-loco (new robo-loco%
                             (nmbs this)
                             (loco loco)
                             (route route))))
        (unless (send robo-loco start)
          (eprintf "nmbs%:route: ~a failed to reach destination ~a~%"
                   loco-id
                   (send dest get-id)))))

    ; Update detection block statuses & loco speeds on regular intervals.
    (define (get-updates)
      (sleep 1)
      (for ((db (in-list (send infrabel get-d-block-statuses))))
        (for ((notify (in-list d-block-listeners)))
          (notify (car db) (cdr db))))
      (for ((loco (in-list (get-loco-ids))))
        (for ((notify (in-list (hash-ref loco-speed-listeners loco '()))))
          (notify (get-loco-speed loco))))
      (get-updates))

    (define/public (stop)
      (send infrabel stop)
      (kill-thread (current-thread)))

    (define update-thread #f)

    ; Calling this method will initialise the nmbs% instance
    ; and then start up the application, including GUI.
    (define/public (start (setup-id #f))
      (define (_start)
        (send infrabel initialize setup-id)
        (set! railway (make-object railway% setup-id))
        (set! starting-spots (find-starting-spots infrabel railway))
        (new window%
             (nmbs this)
             (atexit (lambda () (send this stop))))
        (send infrabel start)
        ;; add callback to switches to notify listeners & infrabel when changed
        (for ((switch (in-list (send railway get-switches))))
          (send switch
                set-callback
                (lambda (id pos)
                  (for ((listener (in-list switch-listeners)))
                    (listener id pos))
                  (send infrabel set-switch-position id pos))))
        (sleep 0.5)
        (set! update-thread (thread get-updates)))

      ; If there's no setup yet, open the setup window.
      (void (if setup-id
              (_start)
              (new setup-window%
                   (setups setup-ids)
                   (callback (lambda (id)
                               (set! setup-id id)
                               (_start)))))))))


;; Simple struct that defines a spot where a locomotive can be added
;; like in the simulator, it needs a track for the train to start on
;; and a connected track to determine its direction
;; only detection blocks are allowed as starting tracks
(struct starting-spot (previous current))

(define (find-starting-spots infrabel railway)
  (let* ((db-ids (send infrabel get-d-block-ids))
         (switch-ids (send infrabel get-switch-ids))
         (infrabel-ids (append db-ids switch-ids))
         (spots (for/list ((track-id
                             (in-list (map (lambda (db) (send db get-id))
                                           (send railway get-d-blocks)))))
                  ; first make sure there's a match
                  (and (memq track-id infrabel-ids)
                       (let* ((track (send railway get-track track-id))
                              ; get ids from tracks connected to current track
                              (ids (map (lambda (s) (send s get-id))
                                        (send track get-connected-tracks)))
                              ; check if any connected track can be found in infrabel
                              (prev (findf (lambda (x)
                                             (memq x infrabel-ids))
                                           ids)))
                         ; create & return starting-spot object if result was a match
                         (and prev
                              (cons track-id (starting-spot prev track-id))))))))
    (for/hash ((spot (in-list spots))
               #:when spot)
      (values (car spot) (cdr spot)))))

