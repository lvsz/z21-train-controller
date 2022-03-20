#lang racket/base

(provide run-nmbs)

(require racket/class
         "nmbs/nmbs.rkt"
         "infrabel/infrabel.rkt"
         (prefix-in tcp: "infrabel/client.rkt")
         (prefix-in local: "infrabel/infrabel.rkt"))

(define (run-nmbs (remote #t))

  (define infrabel
    (if remote
      (new tcp:infrabel%)
      (new local:infrabel%)))

  (define nmbs
    (new nmbs% (infrabel infrabel)))

  (send nmbs start))

(module* main #f
  (void (run-nmbs)))

