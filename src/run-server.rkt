#lang racket/base

(require "infrabel/server.rkt")

(define rpi-config "resources/tcp/raspberrypi.txt")
(define local-config "resources/tcp/localhost.txt")
  (define (get-port file)
    (call-with-input-file file (lambda (in)
                                 (string->number (read-line in)))))

(module* main #f
  (let ((host "localhost")
        (port #f)
        (logging 'debug))
    (for ((arg (in-vector (current-command-line-arguments))))
      (case arg
        (("--rpi" "--raspberrypi" "-r")
         (set! host "raspberrypi")
         (set! port (get-port rpi-config)))
        (("--local" "--localhost" "-l")
         (set! host "localhost")
         (set! port (get-port local-config)))
        (("--debug" "-d")
         (set! logging 'debug))
        (("--info" "-i")
         (set! logging 'info))
        (else
         (eprintf "Unrecognized argument: " arg))))
    (unless port
      (set! port (get-port local-config)))
    (void (start-server port logging))))

