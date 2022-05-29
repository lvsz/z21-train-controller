#lang racket/base

(provide infrabel%)

(require racket/class
         racket/list
         "interface.rkt"
         "../railway/railway.rkt"
         (prefix-in z21: "../z21/interface.rkt")
         (prefix-in sim: "../simulator/interface.rkt"))

(define infrabel%
  (class* object% (infrabel-interface<%>)
    (init-field (log-level #f))
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

    (define (simulation-mode setup-id)
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

    (define (z21-mode)
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

    (define initialized #f)

    (define/public (initialize setup-id (mode 'sim))
      (case mode
        ((z21) (z21-mode))
        ((sim) (simulation-mode setup-id))
        (else (error (format "initialize: ~a is not a valid mode" mode))))
      (set! railway (make-object railway% setup-id))
      (ext:start)
      (set! initialized #t))

    (define/public (initialized?)
      initialized)

    (define running #f)

    (define/public (start)
      (for ((switch (in-list (send railway get-switches))))
        ; TODO: set hardware positions instead of getting them
        (send switch set-position (ext:get-switch-position (send switch get-id))))
      (set! running #t)
      (thread (lambda ()
                (let loop ()
                  (for ((loco (in-list (send railway get-locos))))
                    (get-loco-d-block (send loco get-id)))
                  (sleep 1)
                  (when running (loop))))))

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

    ; This procedure keeps track of a loco's movement by
    ; comparing the detection blocks it has traversed.
    (define/public (get-loco-d-block loco-id)
      (let* ((new-d-block-id (ext:get-loco-d-block loco-id))
             (new-d-block (and new-d-block-id (get-track new-d-block-id)))
             (loco (send railway get-loco loco-id))
             (old-db (send loco get-d-block)))
        (cond
          ; Nothing changed
          ((eq? new-d-block old-db)
           (void))
          ; Loco is on a detection block but wasn't before
          ((and new-d-block (not old-db))
           (send new-d-block occupy)
           (send loco update-location new-d-block))
          ; Loco is on a new detection block
          ((and new-d-block old-db)
           (send old-db clear)
           (send new-d-block occupy)
           (send loco update-location new-d-block))
          ; Else loco left a detection block
          (else
           (send old-db clear)
           (send loco left-d-block)))
        new-d-block-id))

    (define/public (get-d-block-ids)
      (ext:get-d-block-ids))
    (define/public (get-switch-ids)
      (ext:get-switch-ids))
    (define/public (get-switch-position id)
      (ext:get-switch-position id))

    (define/public (set-switch-position id position)
      (send (get-track id) set-position position)
      (ext:set-switch-position! id position))

    (define/public (get-d-block-statuses)
      (for/list ((d-block (in-list (send railway get-d-blocks))))
        (cons (send d-block get-id) (send d-block get-status))))

    (define (get-track id)
      (send railway get-track id))

    (define (prevent-collisions)
      (void))))

