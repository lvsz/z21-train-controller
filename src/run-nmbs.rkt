#lang racket/base

(provide run-nmbs)

(require racket/class
         "logger.rkt"
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
(define (run-nmbs #:remote? (remote #f) #:setup (setup #f) #:log (log-level #f))
  (let ((setups (map path->string (directory-list "resources/setups/")))
        (args (current-command-line-arguments)))
    (let loop ((i 0))
      (unless (>= i (vector-length args))
        (case (vector-ref args i)
          (("--hardware")
           (set! setup 'hardware)
           (loop (add1 i)))
          (("--setup")
           (if (< (add1 i) (vector-length args))
             (let ((id (vector-ref args (add1 i))))
               (if (member id setups)
                 (set! setup (string->symbol id))
                 (eprintf "Unrecognized setup: ~a~%" id))
               (loop (+ i 2)))
             (eprintf "No setup given~%")))
          (("--remote" "-r")
           (set! remote #t)
           (loop (add1 i)))
          (("--list" "-l")
           (for-each displayln setups)
           (exit))
          (("--log")
           (let ((level (vector-ref args (add1 i))))
             (case level
               (("info")
                (set! log-level 'info))
               (("debug")
                (set! log-level 'debug))
               (else
                (eprintf "Log level needs to be 'info' or 'debug', got: ~a~%" level)))))
          (("--debug")
           (set! log-level 'debug))
          (else
           (eprintf "Unrecognized argument: ~a~%" (vector-ref args i))
           (loop (add1 i)))))))

  (define infrabel
    (if remote
      (new tcp:infrabel%)
      (new raw:infrabel%)))

  (define nmbs
    (new nmbs% (infrabel infrabel) (log-level log-level)))

  (send nmbs start #:setup-id setup))


;; Doesn't run when imported as a module elsewhere
(module* main #f
  (void (run-nmbs)))

