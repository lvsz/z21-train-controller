#lang racket/base

(require (prefix-in railway:  "test/railway.rkt")
         (prefix-in infrabel: "test/infrabel.rkt")
         ;(prefix-in nmbs:     "test/nmbs.rkt")
         )

(railway:run)
(infrabel:run)
;(nmbs:run)

