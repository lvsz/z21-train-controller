#lang racket/base

(provide run)

(require (prefix-in adts:    "railway/adts.rkt")
         (prefix-in railway: "railway/railway.rkt"))

(define (run)
  (adts:run)
  (railway:run))
