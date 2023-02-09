#lang racket/base

(provide nmbs%)

(require racket/async-channel
         racket/class
         racket/match
         "gui.rkt"
         "robo-loco.rkt"
         "../railway/railway.rkt"
         "../logger.rkt")


;; Logging functions
(define-loggers 'nmbs/nmbs log/w log/i log/d)


;; The nmbs% class communicates with an infrabel% instance to control locos
;; Controlling locos is made possible through a GUI
(define nmbs%
  (class object%
    (init-field infrabel)
    (super-new)

    (define main-thread (current-thread))

    (define railway #f)

    (define-syntax get-track
      (syntax-rules ()
        ((_ id) (send railway get-track id))))

    ; List of functions to be called when a switch changes
    (define switch-listeners '())
    (define/public (add-switch-listener fn)
      (log/d "Adding switch listener")
      (set! switch-listeners (cons fn switch-listeners)))

    ; List of functions to be called when a detection block changes
    (define d-block-listeners '())
    (define/public (add-d-block-listener fn)
      (log/d "Adding d-block listener")
      (set! d-block-listeners (cons fn d-block-listeners)))
    (define (notify-d-block-listeners ids-statuses)
      (for* (((id status) (in-hash ids-statuses))
             (notify (in-list d-block-listeners)))
        (notify id status)))

    ; Hashmap of lists of functions called when a loco changes speed
    (define loco-speed-listeners (make-hash))
    (define/public (add-loco-speed-listener loco-id fn)
      (if (hash-has-key? loco-speed-listeners loco-id)
        (hash-update! loco-speed-listeners loco-id (lambda (fns) (cons fn fns)))
        (hash-set! loco-speed-listeners loco-id (list fn))))


    (define/public (get-loco-ids)
      (get-ids (send railway get-locos)))

    (define/public (get-switch-ids)
      (get-ids (send railway get-switches)))

    (define/public (get-d-block-ids)
      (get-ids (send railway get-d-blocks)))


    (define/public (get-switch-position id)
      (send (send railway get-switch id) get-position))

    ; Set switch position without notifying infrabel
    (define (_set-switch-position id pos)
      (send (send railway get-switch id) set-position pos)
      (for ((lstnr (in-list switch-listeners)))
        (lstnr id pos)))

    ; Set switch position and notify infrabel
    (define/public (set-switch-position id pos)
      (send infrabel set-switch-position id pos)
      (_set-switch-position id pos))

    (define/public (set-switch-track id track-id)
      (let ((switch (send railway get-switch id))
            (track (get-track track-id)))
        (cond ((eq? track (get-field track-1 switch))
               (set-switch-position id 1))
              ((eq? track (get-field track-2 switch))
               (set-switch-position id 2))
              ((memq track (get-field options switch))
               (let rec ((s (get-field superior track))
                         (t track))
                 (let ((ss (get-field superior s)))
                   (unless (eq? s ss)
                     (rec ss s))
                   (set-switch-track (get-id s) (get-id t)))))
              (else
               (log/w "set-switch-track:" track-id "not part of" id)))))


    (define (get-loco id)
      (send railway get-loco id))

    (define/public (get-loco-speed id)
      (abs (send infrabel get-loco-speed id)))

    ; Change the speed, notifying any listeners but not infrabel
    (define (_set-loco-speed id speed)
      (send (get-loco id) set-speed speed)
      (for ((notify (in-list (hash-ref loco-speed-listeners id '()))))
        (notify speed)))

    ; Change the speed, notifying any listeners and infrabel
    (define/public (set-loco-speed id speed)
      (send infrabel
            set-loco-speed
            id
            (* (send (get-loco id) get-direction) speed))
      (_set-loco-speed id speed))

    (define/public (change-loco-direction id)
      (let ((loco (get-loco id)))
        (send loco change-direction)
        (send infrabel
              set-loco-speed
              id
              (* (send loco get-direction) (send loco get-speed)))))

    (define/public (add-loco spot-id)
      (let* ((spot (hash-ref starting-spots spot-id))
             (curr-id (starting-spot-current spot))
             (prev-id (starting-spot-previous spot))
             (loco-id (gensym "L")))
        (send infrabel add-loco loco-id prev-id curr-id)
        (send railway add-loco
              loco-id
              (get-track prev-id)
              (get-track curr-id))
        loco-id))

    (define/public (remove-loco loco-id)
      (thread-send
        update-thread
        (lambda ()
          (send infrabel remove-loco loco-id)
          (send railway remove-loco loco-id)
          (hash-remove! loco-speed-listeners loco-id))))

    (define/public (get-loco-d-block id)
      (send infrabel get-loco-d-block id))

    (define/public (update-loco-location loco-id curr-id (prev-id #f))
      (if prev-id
        (send (get-loco loco-id)
              update-location
              (get-track curr-id)
              (get-track prev-id))
        (send (get-loco loco-id)
              update-location
              (get-track curr-id))))

    ; Get hash of spots where a new loco can be added
    ; Any detection block connected to either another block or a switch is valid
    (define starting-spots #f)

    ; Return a list of detection block ids that can be used to place a loco
    (define/public (get-starting-spots)
      (if starting-spots
        (hash-keys starting-spots)
        '()))

    (define/public (route loco-id dest)
      (let* ((loco  (get-loco loco-id))
             (from  (send loco get-location))
             (to    (send railway get-d-block dest))
             (route (send railway get-route from to))
             (on-finish
               (lambda ()
                 (let ((loc (send loco get-location)))
                   (if (eq? loc to)
                     (log/i "Loco" loco "arrived successfully at" to)
                     (log/w "Route failure:" 'loco loco "stopped at" loc
                            "after failing to reach" to
                            "and starting from" from)))))
             (robo-loco
               (and route
                    (new robo-loco%
                         (nmbs this)
                         (loco loco)
                         (route route)
                         (on-finish on-finish)))))
        (log/d "Planned route for loco" loco ': route)
        (if robo-loco
          (begin (log/i "Loco" loco "starting route from" from 'to to)
                 (send robo-loco start))
          (log/i "Invalid route for loco" loco-id 'from from 'to dest))))


    ; Update detection block statuses & loco speeds on regular intervals
    ; Methods prefixed with an underscore indicate private alternative methods
    ; These are used in order to not notify infrabel of changes it sent itself
    (define (get-updates)
      ; Updates could come as lambdas from other threads
      ; Or as updates sent from infrabel
      (let ((update (sync (choice-evt (thread-receive-evt)
                                      (send infrabel get-update)))))
        (log/d "get-updates sync result:" update)
        (match update
          ((? evt?) ; `thread-receive-evt` synchronizes to itself
           ((thread-receive))) ; Call the received lambda
          ((list 'switch id pos) ; Infrabel sent switch position update
           (_set-switch-position id pos))
          ((list 'loco-speed id speed) ; Infrabel sent loco speed update
           (let* ((loco      (get-loco id))
                  (direction (send loco get-direction)))
             ; Change direction if loco stopped while going backwards
             ; Or when new speed has a different sign than direction
             (when (or (and (zero? speed) (negative? direction))
                       (negative? (* speed direction)))
               (send loco change-direction)))
           (_set-loco-speed id (abs speed)))
          ((list 'd-block id 'occupy) ; Infrabel sent d-block status update
           (notify-d-block-listeners (send (get-track id) occupy)))
          ((list 'd-block id 'clear) ; Infrabel sent d-block status update
           (notify-d-block-listeners (send (get-track id) clear)))
          ((list 'kill msg)
           (log/i "Disconnected server:" msg)
           (_stop))
          ((list 'initialized)
           (log/i "Infrabel initialized"))
          (_ (log/w "Unrecognized update format:" update))))
      (get-updates))

    (define (_stop)
      (log/i "Shutting down nmbs")
      (kill-thread main-thread))

    (define/public (stop)
      (send infrabel stop))

    (define update-thread #f)

    ; Calling this method will initialise the nmbs% instance
    ; and then start up the application, including GUI.
    (define/public (start #:setup-id (setup-id #f) #:gui (gui #t))
      (define (_start)
        (send infrabel start)
        (set! railway (make-object railway% setup-id))
        (set! starting-spots (find-starting-spots this railway))
        ; Request correct switch position from infrabel
        (for ((switch-id (in-list (get-switch-ids))))
          (_set-switch-position
            switch-id
            (send infrabel get-switch-position switch-id)))
        (when gui
          (new window%
               (nmbs this)
               (atexit (lambda () (send this stop)))))
        (for ((id (in-list (get-d-block-ids)))
              #:when (eq? 'red (send infrabel get-d-block-status id)))
          (notify-d-block-listeners (send (get-track id) occupy)))
        (set! update-thread (thread get-updates)))
      ; If there's no setup yet, open the setup window.
      (let ((infra-setup (send infrabel get-setup)))
        (cond
          ; Prioritize infrabel's setup if it has one
          (infra-setup
           (set! setup-id infra-setup)
           (log/i "Starting using infrabel's setup:" infra-setup)
           (_start))
          ; Otherwise use given setup if one was provided
          (setup-id
           (send infrabel initialize setup-id)
           (log/i "Starting using given setup:" setup-id)
           (_start))
          ; Else create window to select setup
          (gui
           (void (new setup-window%
                      (setups setup-ids)
                      (callback (lambda (id)
                                  (set! setup-id id)
                                  (send infrabel initialize id)
                                  (log/i "Starting using selected setup:" id)
                                  (_start)))))))))))


;; Simple struct that defines a spot where a locomotive can be added
;; It needs a track for the train to start on
;; And a connected track to determine its direction
;; Only detection blocks are allowed as starting tracks
(struct starting-spot (previous current))

(define (find-starting-spots nmbs railway)
  (let* ((db-ids       (send nmbs get-d-block-ids))
         (track-ids    (append db-ids (send nmbs get-switch-ids)))
         (spots
           (for/list ((db-id (in-list db-ids)))
             (let* ((db (send railway get-track db-id))
                    ; Get ids from tracks connected to current track
                    (ids (get-ids (send db get-connected-tracks)))
                    ; Check if any connected track can be found
                    (prev-id (findf (lambda (x) (memq x track-ids)) ids)))
               ; Return id & a struct if result was a match
               (and prev-id
                    (cons db-id (starting-spot prev-id db-id)))))))
    (for/hash ((spot (in-list spots))
               #:when spot)
      (values (car spot) (cdr spot)))))

