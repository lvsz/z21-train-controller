#lang racket/base

(provide infrabel-interface<%>)

(require racket/class
         racket/contract/base)

(define infrabel-interface<%>
  (interface ()
    (start (->m void?))
    (stop  (->m any))
    (initialize   (->m symbol? void?))
    (get-setup    (->m (or/c symbol? #f)))
    (add-loco     (->m symbol? symbol? symbol? void?))
    (remove-loco  (->m symbol? void?))
    (get-loco-speed (->m symbol? integer?))
    (set-loco-speed (->m symbol? integer? void?))
    (get-loco-d-block (->m symbol? (or/c symbol? #f)))
    (get-switch-position  (->m symbol? (or/c 1 2)))
    (set-switch-position  (->m symbol? (or/c 1 2) void?))
    (get-update (->m evt?))))

