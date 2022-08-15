#lang racket/base

(provide run)

(require (prefix-in tracks:  "railway/tracks.rkt")
         (prefix-in railway: "railway/railway.rkt"))

(define (run)
  (displayln "Running railway tests")=
  (tracks:run)
  (railway:run))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run))

