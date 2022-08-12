#lang racket/base

(require "infrabel/server.rkt"
         "logger.rkt")

;; 'localhost' confiuguration file
(define local-config "resources/tcp/localhost.txt")

;; 'raspberry pi' confiuguration file
(define rpi-config   "resources/tcp/raspberrypi.txt")

;; List of setup names
(define setups (map path->string (directory-list "resources/setups/")))

;; Get port number from configuration file
(define (get-port file)
  (call-with-input-file file (lambda (in)
                               (string->number (read-line in)))))

;; Text to display for command line usage
(define help
  "usage: run-server [-p | --port NUM]
                  [-h | --host localhost|rpi]
                  [-s | --setup NAME|list]
                  [-m | --mode sim|z21]
                  [-l | --log info|debug]\n")


;; Run server, configuring it with given arguments
(define (run-server #:host  (host "localhost")
                    #:port  (port #f)
                    #:log   (log-level 'info)
                    #:setup (setup #f)
                    #:mode  (mode 'sim))
  (let ((args (current-command-line-arguments)))
    (define (display-setups out)
      (fprintf out "setups: ~a~%" (car setups))
      (for ((s (in-list (cdr setups))))
        (fprintf out "        ~a~%" s)))
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
           (let ((h (vector-ref args i)))
             (case h
               (("local" "localhost")
                (set! host h)
                (unless port
                  (set! port (get-port local-config))))
               (("rpi" "raspberrypi")
                (set! host h)
                (unless port
                  (set! port (get-port rpi-config))))
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
          ((--mode -m)
           (set! i (add1 i))
           (case (string->symbol (vector-ref args i))
             ((sim) (set! mode 'sim))
             ((z21) (set! mode 'z21))
             (else (eprintf "Mode must be 'sim' or 'z21'~%"))))
          ((--help help)
           (display help)
           (exit))
          (else (eprintf "Invalid argument: ~a~%~a" (vector-ref args i) help)))
        (loop (add1 i))))

    ; Get default port if none was set by host or port flag
    (unless port
      (set! port (get-port local-config)))

    (void (start-server port log-level))))

;; Doesn't run when imported as a module elsewhere
(module* main #f
  (run-server))

