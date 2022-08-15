#lang racket/base

(provide run)

(require racket/class
         "infrabel/infrabel.rkt"
         "infrabel/server.rkt"
         "railway/setup.rkt"
         "logger.rkt"
         "resources.rkt")

;; Config that gets used when no other is specified
(define default-config local-config)

;; Print setup names
(define (display-setups out)
  (fprintf out "setups: ~a~%" (car setup-ids))
  (for ((s (in-list (cdr setup-ids))))
    (fprintf out "        ~a~%" s)))

;; Get port number from configuration file
(define (get-port file)
  (call-with-input-file file (lambda (in)
                               (string->number (read-line in)))))

;; Text to display for command line usage
(define help-string
  "usage: infrabel-server [-p | --port NUM]
                       [-h | --host local|rpi]
                       [-s | --setup NAME|list]
                       [-l | --log info|debug|warning|none]\n")

;; Print help string and exit
(define (help out)
  (display help-string out)
  (exit))


;; Run server, configuring it with given arguments
(define (run #:host      (host #f)
             #:port      (port #f)
             #:log-level (log-level 'warning)
             #:setup     (setup #f))
  (let ((args (current-command-line-arguments)))
    (let loop ((i 0))
      (when (< i (vector-length args))
        (case (string->symbol (vector-ref args i))
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
             (help (current-output-port))
             (exit))
           (let ((h (string->symbol (vector-ref args i))))
             (case h
               ((localhost local raspberrypi rpi)
                (set! host h))
               (else (eprintf "Invalid host, try 'local' or 'rpi'~%")))))
          ((--log -l)
           (set! i (add1 i))
           (let ((level (string->symbol (vector-ref args i))))
             (case level
               ((debug info warning none)
                (set! log-level level))
               (else
                (eprintf "Log level must be 'info', 'debug' or 'warning'~%")))))
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
          ((--help help)
           (help (current-output-port)))
          (else (eprintf "Invalid argument: ~a~%" (vector-ref args i))
                (help (current-error-port))))
        (loop (add1 i))))

    ; Get default port if none was set by host or port flag
    (unless port
      (set! port (get-port (case host
                             ((localhost local) local-config)
                             ((raspberrypi rpi) rpi-config)
                             (else              default-config)))))

    (let ((infrabel (new infrabel% (log-level log-level))))
      (void (start-server port #:infrabel infrabel #:setup setup)))))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run))

