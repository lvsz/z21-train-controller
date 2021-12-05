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
        (send switch set-position (ext:get-switch-position (send switch get-id))))
      (set! running #t)
      (thread (lambda ()
                (let loop ()
                  (for ((loco (in-list (send railway get-loco-ids))))
                    (get-loco-d-block loco))
                  (sleep 1)
                  (when running (loop))))))

    (define/public (stop)
      (set! running #f)
      (ext:stop-simulator))

    (define/public (set-loco-speed id speed)
      (ext:set-loco-speed id speed))

    (define/public (set-switch-position id position)
      (send (get-track id) (set-position position))
      (ext:set-switch-position! id position))

    (define/public (get-d-block-statuses)
      (for/list ((d-block (in-list (send railway get-d-blocks))))
        (cons (send d-block get-id) (send d-block get-status))))))

