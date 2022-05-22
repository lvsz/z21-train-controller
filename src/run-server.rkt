#lang racket/base

(require "infrabel/server.rkt"
         "logger.rkt")

(define rpi-config "resources/tcp/raspberrypi.txt")
(define local-config "resources/tcp/localhost.txt")
  (define (get-port file)
    (call-with-input-file file (lambda (in)
                                 (string->number (read-line in)))))

(module* main #f
  (let ((host "localhost")
        (port #f)
        (log-level 'debug))
    (for ((arg (in-vector (current-command-line-arguments))))
      (case arg
        (("--rpi" "--raspberrypi" "-r")
         (set! host "raspberrypi")
         (set! port (get-port rpi-config)))
        (("--local" "--localhost" "-l")
         (set! host "localhost")
         (set! port (get-port local-config)))
        (("--debug" "-d")
         (set! log-level 'debug))
        (("--info" "-i")
         (set! log-level 'info))
        (else
         (eprintf "Unrecognized argument: " arg))))
    (unless port
      (set! port (get-port local-config)))
    (void (start-server port log-level))))

