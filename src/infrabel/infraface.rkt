#lang racket/base

(provide infraface<%>)

(require racket/class
         racket/contract/base)

(define infraface<%>
  (interface ()
    (start (->m void?))
    (stop  (->m any))
    (initialize   (->m symbol? void?))
    (get-setup    (->m (or/c symbol? #f)))
    (add-loco     (->m symbol? symbol? symbol? void?))
    (remove-loco  (->m symbol? void?))
    (get-loco-speed (->m symbol? integer?))
    (set-loco-speed (->m symbol? integer? void?))
    (get-loco-d-block   (->m symbol? (or/c symbol? #f)))
    (get-d-block-ids    (->m (listof symbol?)))
    (get-d-block-status (->m symbol? symbol?))
    (get-switch-ids     (->m (listof symbol?)))
    (get-switch-position  (->m symbol? (or/c 1 2)))
    (set-switch-position  (->m symbol? (or/c 1 2) void?))
    (get-update (->m evt?))))
