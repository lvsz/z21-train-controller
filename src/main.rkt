#lang racket/base

(require racket/class
         "nmbs/nmbs.rkt"
         "infrabel/infrabel.rkt")

(define (main)

  (define infrabel
    (new infrabel%))

  (define nmbs
    (new nmbs% (infrabel infrabel)))

  (send nmbs start 'hardware))

(main)

