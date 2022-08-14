#lang racket/base

(require "infrabel/server.rkt"
         "logger.rkt")

;; 'localhost' confiuguration file
(define local-config "resources/tcp/localhost.txt")

;; 'raspberry pi' confiuguration file
(define rpi-config   "resources/tcp/raspberrypi.txt")

;; Config that gets used when no other is specified
(define default-config local-config)

;; List of setup names
(define setups (map path->string (directory-list "resources/setups/")))

;; Print setup names
(define (display-setups out)
  (fprintf out "setups: ~a~%" (car setups))
  (for ((s (in-list (cdr setups))))
    (fprintf out "        ~a~%" s)))

;; Get port number from configuration file
(define (get-port file)
  (call-with-input-file file (lambda (in)
                               (string->number (read-line in)))))

;; Text to display for command line usage
(define help-string
  "usage: run-nmbs [-p | --port NUM]
                [-h | --host local|rpi]
                [-s | --setup NAME|list]
                [-l | --log info|debug]\n")

;; Print help string and exit
(define (help out)
  (display help-string out)
  (exit))


;; Run server, configuring it with given arguments
(define (run-server #:host  (host #f)
                    #:port  (port #f)
                    #:log   (log-level 'warning)
                    #:setup (setup #f))
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
             (display help)
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
               ((debug info)
                (set! log-level level))
               (else (eprintf "Log level must be 'info' or 'debug'~%")))))
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

    (void (start-server port #:log log-level #:setup setup))))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run-server))

