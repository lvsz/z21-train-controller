#lang racket/base

(provide run)

(require (prefix-in server:  "infrabel/server.rkt"))

(define (run)
  (displayln "Running infrabel tests")
  (server:run))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run))

