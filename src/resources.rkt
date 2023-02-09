#lang racket/base

(provide setup-location
         tcp-files
         local-config
         default-config)

(define source (let-values (((root dir b) (split-path (current-directory))))
                 (if (equal? dir (string->path "src"))
                   "resources"
                   "src/resources")))

(define setup-location (build-path source "setups"))

(define tcp-files (directory-list (build-path source "tcp") #:build? #t))
(define local-config (build-path source "tcp/localhost.txt"))
(define default-config   (build-path source "tcp/raspberrypi.txt"))

