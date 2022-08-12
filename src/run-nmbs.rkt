#lang racket/base

(provide run-nmbs)

(require racket/class
         "logger.rkt"
         "nmbs/nmbs.rkt"
         "infrabel/client.rkt"
         "infrabel/infrabel.rkt")

;; List of setup names
(define setups (map path->string (directory-list "resources/setups/")))

;; Print setup names
(define (display-setups out)
  (fprintf out "setups: ~a~%" (car setups))
  (for ((s (in-list (cdr setups))))
    (fprintf out "        ~a~%" s)))

;; Text to display for command line usage
(define help-string
  "usage: run-nmbs [-p | --port NUM]
                [-s | --setup NAME|list]
                [-l | --log info|debug]\n")

;; Print help string and exit
(define (help out)
  (display help-string out)
  (exit))


;; This starts up the program.
;;   if #:local? = #t : everything runs in a single instance
;;   if #:local? = #f : it needs to connect with a server
;;     the server can be either localhost or raspberypi
;;   if #:setup = #f : gives the user a selection screen to pick a setup
;;   otherwise it will skip that step (assuming the given setup exists)
(define (run-nmbs #:local? (local #f)
                  #:setup  (setup #f)
                  #:log    (log-level 'info))
  (let ((args (current-command-line-arguments)))
    (let loop ((i 0))
      (when (< i (vector-length args))
        (case (string->symbol (vector-ref args i))
          ((--setup -s)
           (set! i (add1 i))
           (let ((arg (vector-ref args i)))
             (cond ((string=? arg "list")
                    (display-setups (current-output-port))
                    (exit))
                   ((member arg setups)
                    (set! setup (string->symbol arg)))
                   (else (eprintf "Invalid setup: ~a~%" arg)
                         (display-setups (current-error-port))))))
          ((--log -l)
           (set! i (add1 i))
           (let ((level (string->symbol (vector-ref args i))))
             (case level
               ((debug info)
                (set! log-level level))
               (else (eprintf "Log level must be 'info' or 'debug'~%")))))
          ((--help -h help)
           (help (current-output-port)))
          (else (eprintf "Invalid argument: ~a~%" (vector-ref args i))
                (help (current-error-port))))
        (loop (add1 i)))))

  (define infrabel
    (if local
      (new infrabel%)
      (new infrabel-client%)))

  (define nmbs
    (new nmbs% (infrabel infrabel) (log-level log-level)))

  (send nmbs start #:setup-id setup))


;; Doesn't run when imported as a module elsewhere
(module* main #f
  (void (run-nmbs)))

