#lang racket/base

(provide run)

(require (prefix-in nmbs:  "nmbs/nmbs.rkt"))

(define (run)
    (nmbs:run))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run))

