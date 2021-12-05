#lang racket/base

(require racket/list
         racket/sequence
         racket/string)

(provide setup-ids
         get-setup
         read-setup)

(define setup-location "resources/setups")

(struct setup (id contents))

(define (read-setup setup)
  ((setup-contents setup)))

(define setup-files (directory-list setup-location #:build? #t))

(define setup-ids (map (lambda (x) (string->symbol (path->string x)))
                       (directory-list setup-location)))

(define (make-setup)
  (filter pair? (map string-split (sequence->list (in-lines)))))

(define setups
  (for/list ((id   (in-list setup-ids))
             (file (in-list setup-files)))
    (setup id (lambda ()
                (with-input-from-file file make-setup #:mode 'text)))))

(define (get-setup id)
  (let ((setup (findf (lambda (s) (eq? id (setup-id s))) setups)))
    (if setup
      setup
      (error (format "No setup named ~a found" id)))))

