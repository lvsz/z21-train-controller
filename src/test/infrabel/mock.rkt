#lang racket/base

(provide infrabel-mock%)

(require racket/class
         racket/async-channel
         "../../infrabel/infraface.rkt"
         "../../railway/railway.rkt")

(define infrabel-mock%
  (class* object% (infraface<%>)
    (super-new)

    (define running #f)
    (define railway #f)
    (define setup #f)

    (define/public (test-running)
      running)

    (define/public (test-railway)
      railway)

    (define/public (start)
      (set! running #t))

    (define/public (stop)
      (set! running #f))

    (define/public (initialize setup-id)
      (set! setup setup-id)
      (set! railway (make-object railway% setup-id))
      (send-update 'initialized))

    (define/public (get-setup)
      setup)

    (define loco-speeds (make-hash))

    (define/public (add-loco id prev-id curr-id)
      (let ((prev (send railway get-track prev-id))
            (curr (send railway get-track curr-id)))
        (hash-set! loco-speeds id 0)
        (send railway add-loco id prev curr)))

    (define/public (remove-loco id)
      (send railway remove-loco id))

    (define/public (get-loco-speed id)
      (hash-ref loco-speeds id))

    (define/public (set-loco-speed id speed)
      (hash-set! loco-speeds id speed))

    (define/public (test-set-loco-speed id speed)
      (set-loco-speed id speed)
      (send-update 'loco-speed id speed))

    (define/public (get-loco-d-block id)
      (let ((loco (send railway get-loco id)))
        (and loco
             (let ((track (send loco get-d-block)))
               (and track (get-id track))))))

    (define/public (get-switch-position id)
      (send (send railway get-track id) get-position))

    (define/public (set-switch-position id p)
      (send (send railway get-track id) set-position p))

    (define/public (test-set-switch-position id p)
      (set-switch-position id p)
      (send-update 'switch id p))

    (define/public (get-switch-ids)
      (get-ids (send railway get-switches)))

    (define/public (get-d-block-ids)
      (get-ids (send railway get-d-blocks)))

    (define/public (get-d-block-status id)
      (send (send railway get-track id) get-status))

    (define update-channel (make-async-channel))
    (define (send-update tag . args)
      (async-channel-put update-channel (cons tag args)))

    (define/public (get-update)
      update-channel)))
