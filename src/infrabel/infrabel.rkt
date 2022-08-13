#lang racket/base

(provide infrabel%)

(require racket/async-channel
         racket/class
         racket/list
         "interface.rkt"
         "../logger.rkt"
         "../railway/railway.rkt"
         (prefix-in z21: "../z21/interface.rkt")
         (prefix-in sim: "../simulator/interface.rkt"))

;; Logger functions
(define-loggers 'infrabel/infrabel log/w log/i log/d)

(define infrabel%
  (class* object% (infrabel-interface<%>)
    (super-new)

    (define railway #f)

    (define ext:start                void)
    (define ext:stop                 void)
    (define ext:get-loco-d-block     void)
    (define ext:get-loco-speed       void)
    (define ext:set-loco-speed!      void)
    (define ext:set-switch-position! void)
    (define ext:get-switch-position  void)
    (define ext:add-loco             void)
    (define ext:remove-loco          void)
    (define ext:get-d-block-ids      void)
    (define ext:get-switch-ids       void)

    (define (simulation-mode! setup-id)
      (case setup-id
        ((hardware)             (sim:setup-hardware))
        ((straight)             (sim:setup-straight))
        ((straight-with-switch) (sim:setup-straight-with-switch))
        ((loop)                 (sim:setup-loop))
        ((loop-and-switches)    (sim:setup-loop-and-switches))
        (else (error (format "Setup ~a not found" setup-id))))
      (set! ext:start                sim:start)
      (set! ext:stop                 sim:stop)
      (set! ext:get-loco-d-block     sim:get-loco-detection-block)
      (set! ext:get-loco-speed       sim:get-loco-speed)
      (set! ext:set-loco-speed!      sim:set-loco-speed!)
      (set! ext:set-switch-position! sim:set-switch-position!)
      (set! ext:get-switch-position  sim:get-switch-position)
      (set! ext:add-loco             sim:add-loco)
      (set! ext:remove-loco          sim:remove-loco)
      (set! ext:get-d-block-ids      sim:get-detection-block-ids)
      (set! ext:get-switch-ids       sim:get-switch-ids))

    (define (z21-mode!)
      (set! ext:start                z21:start)
      (set! ext:stop                 z21:stop)
      (set! ext:get-loco-d-block     z21:get-loco-detection-block)
      (set! ext:get-loco-speed       z21:get-loco-speed)
      (set! ext:set-loco-speed!      z21:set-loco-speed!)
      (set! ext:set-switch-position! z21:set-switch-position!)
      (set! ext:get-switch-position  z21:get-switch-position)
      (set! ext:add-loco             z21:add-loco)
      (set! ext:remove-loco          void)
      (set! ext:get-d-block-ids
            (lambda () (send railway get-d-block-ids)))
      (set! ext:get-switch-ids
            (lambda () (send railway get-switch-ids))))

    (define setup #f)

    (define/public (initialize setup-id)
      (set! setup setup-id)
      (if (eq? setup-id 'z21)
        (z21-mode!)
        (simulation-mode! setup-id))
      (set! railway (make-object railway% setup-id))
      (ext:start)
      (send-update 'initialized))


    (define/public (get-setup)
      setup)

    (define running #f)

    (define/public (start)
      (define (infrabel-loop)
        (for ((loco (in-list (send railway get-locos))))
          (get-loco-d-block (get-id loco)))
        (sleep 0.5)
        (when running
          (infrabel-loop)))
      (unless running
        (for ((switch (in-list (send railway get-switches))))
          (send switch
                set-position
                (ext:get-switch-position (send switch get-id))))
        (set! running #t)
        (void (thread infrabel-loop))))

    (define/public (stop)
      (set! running #f)
      (ext:stop))

    (define/public (add-loco id prev curr)
      (define prev-track (get-track prev))
      (define curr-track (get-track curr))
      (ext:add-loco id prev curr)
      (send railway add-loco id prev-track curr-track)
      (send curr-track occupy))

    (define/public (remove-loco id)
      (let ((d-block (send (send railway get-loco id) get-d-block)))
        (when d-block (send d-block clear)))
      (ext:remove-loco id)
      (send railway remove-loco id))

    (define/public (get-loco-speed id)
      (ext:get-loco-speed id))

    (define/public (set-loco-speed id speed)
      (ext:set-loco-speed! id speed))

    ; This procedure keeps track of a loco's movement
    ; It compares the detection blocks it has traversed
    (define/public (get-loco-d-block loco-id)
      (define (occupy d-block)
        (log/d "D-block status update:" d-block 'occupy)
        (send d-block occupy)
        (send-update 'd-block (get-id d-block) 'occupy))
      (define (clear d-block)
        (log/d "D-block status update:" d-block 'clear)
        (send d-block clear)
        (send-update 'd-block (get-id d-block) 'clear))
      (let* ((loco      (send railway get-loco loco-id))
             (old-db    (send loco get-d-block))
             (new-db-id (ext:get-loco-d-block loco-id))
             (new-db    (and new-db-id (get-track new-db-id))))
        (cond
          ; Nothing changed
          ((eq? new-db old-db)
           (log/d "No d-block update for" loco 'on old-db))
          ; Loco is on a new detection block
          (new-db
           (log/i "Loco" loco "entered" new-db)
           (occupy new-db)
           (send loco update-location new-db)
           ; Clear previous d-block if there was one
           (when old-db
             (log/i 'Loco loco 'left old-db)
             (clear old-db)))
          ; Else loco left a detection block
          (else
           (log/i 'Loco loco 'left old-db)
           (clear old-db)
           (send loco left-d-block)))
        new-db-id))

    (define/public (get-d-block-ids)
      (ext:get-d-block-ids))
    (define/public (get-switch-ids)
      (ext:get-switch-ids))
    (define/public (get-switch-position id)
      (ext:get-switch-position id))

    (define/public (set-switch-position id position)
      (let* ((switch (send railway get-switch id))
             (sup-switch (get-field superior switch))
             (track (if (= position 1)
                      (get-field track-1 switch)
                      (get-field track-2 switch))))
        ; If switch is part of another switch, switch other switch first
        (unless (eq? switch sup-switch)
          (let ((s-id (send sup-switch get-id))
                (s-pos (if (eq? (get-field track-1 sup-switch) switch)
                         1
                         2)))
            (set-switch-position s-id s-pos)))
        ; Skip when switch is already in the correct position
        (unless (eq? position
                     (send switch get-position)
                     (ext:get-switch-position id))
          (send-update 'switch id position)
          (send switch set-position position)
          (ext:set-switch-position! id position))))

    (define/public (get-d-block-statuses)
      (for/list ((d-block (in-list (send railway get-d-blocks))))
        (cons (send d-block get-id) (send d-block get-status))))

    (define update-channel (make-async-channel))
    (define (send-update tag . args)
      (async-channel-put update-channel (cons tag args)))
    (define/public (get-update)
      update-channel)

    (define (get-track id)
      (send railway get-track id))))

