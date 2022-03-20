#lang racket/base

(provide run-nmbs)

(require racket/class
         "nmbs/nmbs.rkt"
         "infrabel/infrabel.rkt"
         (prefix-in tcp: "infrabel/client.rkt")
         (prefix-in raw: "infrabel/infrabel.rkt"))


;; The starts up the program.
;;   if #:remote? = #f : everything runs in a single instance
;;   if #:remote? = #t : it needs to connect with a server
;;     the server can be either localhost or raspberypi
;;   if #:setup = #f : gives the user a selection screen to pick a setup
;;   otherwise it will skip that step (assuming the given setup exists)
(define (run-nmbs #:remote? (remote #f) #:setup (setup #f))
  (for ((arg (in-vector (current-command-line-arguments))))
    (case arg
      (("--hardware" "-h")
       (set! setup 'hardware))
      (("--remote" "-r")
       (set! remote #t))
      (else
       (eprintf "Unrecognized argument: ~a~%" arg))))

  (define infrabel
    (if remote
      (new tcp:infrabel%)
      (new raw:infrabel%)))

  (define nmbs
    (new nmbs% (infrabel infrabel)))

  (send nmbs start setup))


;; Doesn't run when imported as a module elsewhere
(module* main #f
  (void (run-nmbs)))

