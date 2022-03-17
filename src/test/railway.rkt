#lang racket/base

(provide run)

(require (prefix-in tracks:    "railway/tracks.rkt")
         (prefix-in railway: "railway/railway.rkt"))

(define (run)
    (tracks:run)
    (railway:run))
