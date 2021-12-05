#lang racket/base

(provide setup-ids
         get-setup
         read-setup)

(require racket/list
         racket/sequence
         racket/string)

(define setup-location "resources/setups")

;; Stores the name of the setup as a symbol and the contents as a procedure
;; that can be called to read the associated files.
(struct setup (id contents))

;; Reads the contents of a given setup struct
(define (read-setup setup)
  ((setup-contents setup)))

;; Lists the files found in resource/setups.
(define setup-files (directory-list setup-location #:build? #t))

;; Lists the names of the files.
(define setup-ids (map (lambda (x) (string->symbol (path->string x)))
                       (directory-list setup-location)))

;; Reads input line by line, then splits every line
;; to evaluate later on, and filtering out any empty lines.
(define (get-contents)
  (filter pair? (map string-split (sequence->list (in-lines)))))

;; List of all available setup structs.
(define setups
  (for/list ((id   (in-list setup-ids))
             (file (in-list setup-files)))
    (setup id (lambda ()
                (with-input-from-file file get-contents #:mode 'text)))))

;; Get a specific setup.
(define (get-setup id)
  (let ((setup (findf (lambda (s) (eq? id (setup-id s))) setups)))
    (if setup
      setup
      (error (format "No setup named ~a found" id)))))

