#lang racket/base

(provide infrabel%)

(require racket/class
         racket/list
         "../railway/railway.rkt"
         (prefix-in sim: "../simulator/interface.rkt"))

(define infrabel%
  (class object%
    (super-new)

    (define railway #f)

    (define ext:start-simulator      sim:start)
    (define ext:stop-simulator       sim:stop)
    (define ext:get-loco-d-block     sim:get-loco-detection-block)
    (define ext:get-loco-speed       sim:get-loco-speed)
    (define ext:set-loco-speed!      sim:set-loco-speed!)
    (define ext:set-switch-position! sim:set-switch-position!)
    (define ext:get-switch-position  sim:get-switch-position)
    (define ext:add-loco             sim:add-loco)
    (define ext:remove-loco          sim:remove-loco)
    (define ext:get-d-block-ids      sim:get-detection-block-ids)
    (define ext:get-switch-ids       sim:get-switch-ids)

    (define/public (initialize setup-id)
      (set! railway (make-object railway% setup-id))
      (case setup-id
          ((hardware)             (sim:setup-hardware))
          ((straight)             (sim:setup-straight))
          ((straight-with-switch) (sim:setup-straight-with-switch))
          ((loop)                 (sim:setup-loop))
          ((loop-and-switches)    (sim:setup-loop-and-switches))
          (else                   (error (format "Setup ~a not found" setup-id))))
      (ext:start-simulator)
      'initialized)

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
      (ext:stop-simulator))

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
      (send railway get-track id))))

