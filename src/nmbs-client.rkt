#lang racket/base

(provide run)

(require racket/class
         "logger.rkt"
         "nmbs/nmbs.rkt"
         "infrabel/client.rkt"
         "infrabel/infrabel.rkt"
         "railway/setup.rkt")

;; Print setup names
(define (display-setups out)
  (fprintf out "setups: ~a~%" (car setup-ids))
  (for ((s (in-list (cdr setup-ids))))
    (fprintf out "        ~a~%" s)))

;; Text to display for command line usage
(define help-string
  "usage: nmbs-client [-s | --setup NAME|list]
                   [-p | --port NUM]
                   [-h | --host HOSTNAME]
                   [-l | --log info|debug|warning|none]
                   [--solo]\n")

;; Print help string and exit
(define (help out)
  (display help-string out)
  (exit))


;; This starts up the program.
;;   if #:solo? = #t : everything runs in a single instance
;;   if #:solo? = #f : it needs to connect with a server
;;     the server can be either localhost or raspberypi
;;   if #:setup = #f : gives the user a selection screen to pick a setup
;;   otherwise it will skip that step (assuming the given setup exists)
(define (run #:setup     (setup #f)
             #:port      (port #f)
             #:host      (host "localhost")
             #:solo?     (solo #f)
             #:log-level (log-level 'warning))
  (let ((args (current-command-line-arguments)))
    (let loop ((i 0))
      (when (< i (vector-length args))
        (case (string->symbol (vector-ref args i))
          ((--setup -s)
           (set! i (add1 i))
           (let ((arg (string->symbol (vector-ref args i))))
             (cond ((eq? arg 'list)
                    (display-setups (current-output-port))
                    (exit))
                   ((member arg setup-ids)
                    (set! setup arg))
                   (else (eprintf "Invalid setup: ~a~%" arg)
                         (display-setups (current-error-port))))))
          ((--port -p)
           (set! i (add1 i))
           (let ((p (string->number (vector-ref args i))))
             (if (and (integer? p)
                      (<= 0 p 65535))
               (set! port p)
               (eprintf "Port must be integer between 0 and 65535~%"))))
          ((--host -h)
           (set! i (add1 i))
           (when (= i (vector-length args))
             (display help)
             (exit))
           (set! host (vector-ref args i)))
          ((--log -l)
           (set! i (add1 i))
           (let ((level (string->symbol (vector-ref args i))))
             (case level
               ((debug info warning none)
                (set! log-level level))
               (else
                (eprintf "Log level must be 'info', 'debug' or 'warning'~%")))))
          ((--solo)
           (set! solo #t))
          ((--help help)
           (help (current-output-port)))
          (else (eprintf "Invalid argument: ~a~%" (vector-ref args i))
                (help (current-error-port))))
        (loop (add1 i)))))

  (define infrabel
    (if solo
      (new infrabel% (log-level log-level))
      (new infrabel-client% (port port) (host host) (log-level log-level))))

  (define nmbs
    (new nmbs% (infrabel infrabel)))

  (send nmbs start #:setup-id setup))


;; Doesn't run when imported as a module elsewhere
(module* main #f
  (void (run)))

