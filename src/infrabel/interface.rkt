#lang racket/base

(provide infrabel-interface<%>)

(require racket/class
         racket/contract/base)

(define infrabel-interface<%>
  (interface ()
    (start (->m void?))
    (stop  (->m any))
    (initialize   (->m symbol? symbol? void?))
    (initialized? (->m boolean?))
    (add-loco     (->m symbol? symbol? symbol? void?))
    (remove-loco  (->m symbol? void?))
    (get-loco-speed (->m symbol? integer?))
    (set-loco-speed (->m symbol? integer? void?))
    (get-loco-d-block (->m symbol? (or/c symbol? #f)))
    (get-d-block-ids  (->m (listof symbol?)))
    (get-switch-ids   (->m (listof symbol?)))
    (get-switch-position  (->m symbol? (or/c 1 2)))
    (set-switch-position  (->m symbol? (or/c 1 2) void?))
    (get-d-block-statuses (->m (listof pair?)))))
