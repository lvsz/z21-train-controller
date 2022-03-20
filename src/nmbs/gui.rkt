#lang racket/gui

(provide setup-window% window%)

;; maximum loco speed allowed by controller
(define max-speed 300)

(define window%
  (class frame%
    (init-field nmbs (atexit void) (width 400) (height 400))
    (super-new (label "NMBS")
               (width width)
               (height (+ height 22)))
    (define (key-callback)
      (void))
    (define update-callback void)
    (define buffer-bmp (make-object bitmap% width height))
    (define buffer-bmp-dc (make-object bitmap-dc% buffer-bmp))

    (define d-block-ids
      (sort (send nmbs get-d-block-ids) id<?))

    (define/augment (on-close)
      (show #f)
      (atexit))

    (define parent-panel
      (new vertical-panel% (parent this)))

    (define bottom-pane
      (new horizontal-panel%
           (parent parent-panel)
           (alignment '(center top))))

    (define loco-panel
      (new loco-panel%
           (nmbs nmbs)
           (d-block-ids d-block-ids)
           (parent bottom-pane)))

    (define switch-panel
      (new switch-panel%
           (nmbs nmbs)
           (parent bottom-pane)))

    (define d-block-panel
      (new d-block-panel%
           (nmbs nmbs)
           (d-block-ids d-block-ids)
           (parent bottom-pane)))

    (inherit show)
    (show #t)
    (send parent-panel show #t)
    (send loco-panel show #t)))


(define setup-window%
  (class frame%
    (init-field setups callback)
    (init (width 100) (height 100))
    (super-new (label "Pick a setup"))

    (define parent-panel
      (new vertical-panel% (parent this)))

    (define setup-panel
      (new choice%
           (label "Current setup")
           (choices (map symbol->string setups))
           (parent parent-panel)
           (vert-margin 20)
           (callback
             (lambda (choice evt)
               (thread (lambda ()
                         (let ((idx (send choice get-selection)))
                           (when idx
                             (callback (list-ref setups idx))
                             (show #f)))))))))
    (inherit show)
    (show #t)))


;; main panel for the user to control locomotives
(define loco-panel%
  (class panel%
    (init-field nmbs d-block-ids)
    (super-new (enabled #t)
               (style '(border))
               (min-height 300)
               (stretchable-height #f)
               (alignment '(right top)))

    ;; list that keeps all loco ids
    (define locos (send nmbs get-loco-ids))

    ;; no active loco when there are no locos
    ;; otherwise default to first one in list
    (define active-loco
      (if (null?  locos)
        #f
        (car locos)))

    (define (set-active-loco! loco)
      (set! active-loco loco)
      (active-loco-update!))

    ;; function that updates UI elements when active loco changes
    (define (active-loco-update!)
      (cond ((not active-loco)
             (send speed-slider enable #f)
             (send route-choice enable #f))
            ((zero? (send nmbs get-loco-speed active-loco))
             (send speed-slider enable #t)
             (if (send nmbs get-loco-d-block active-loco)
               (send route-choice enable #t)
               (send route-choice enable #f)))
            (else
             (send speed-slider enable #t)
             (send route-choice enable #f))))

    ;; list of track ids that a loco can be added on
    (define starting-spots
      (sort (send nmbs get-starting-spots) id<?))

    ;; make a listener for the speed controller so it can get updated
    ;; when the active loco's speed changes
    (define (mk-listener id)
      (lambda (speed)
        (when (eq? id active-loco)
          (send speed-slider set-value speed))))
    (for ((loco (in-list locos)))
      (send nmbs add-loco-speed-listener loco (mk-listener loco)))

    ;; subpanel used to add new locomotives
    ;; uses a list of tracks that have the same id in both nmbs & simulator
    (define add-loco-menu
      (new choice%
           (label "Add new locomotive to track")
           (choices (cons "---" (map symbol->string starting-spots)))
           (parent this)
           (min-width 155)
           (callback
             (lambda (choice evt)
               (let ((idx (sub1 (send choice get-selection))))
                 (when (>= idx 0)
                   (let ((id (send nmbs add-loco (list-ref starting-spots idx))))
                     ; updates loco list & selection menu
                     (add-loco-to-menu! id)
                     ; add listener for speed updates
                     (send nmbs add-loco-speed-listener id (mk-listener id))
                     ; set new loco to active loco
                     (set-active-loco! id)
                     (send choice set-selection 0))))))))

    ;; initialize selection menu, listing all available locos
    (define loco-select-menu
      (new choice%
           (label "Active loco")
           (choices (if (null? locos)
                      (list "---")
                      (map symbol->string locos)))
           (parent this)
           (vert-margin 40)
           (min-width 155)
           (callback
             (lambda (choice evt)
               (let ((idx (send choice get-selection)))
                 (unless (null? locos)
                   (let ((loco (list-ref locos idx)))
                     (set-active-loco! loco)
                     (send speed-slider set-value
                           (send nmbs get-loco-speed loco)))))))))

    (define (add-loco-to-menu! id)
      (when (empty? locos)
        (send loco-select-menu delete 0))
      (set! locos (append locos (list id)))
      (send loco-select-menu append (symbol->string id))
      (send loco-select-menu set-selection (sub1 (length locos)))
      (active-loco-update!))

    (define (remove-loco-from-menu!)
      (when active-loco
        (set! locos (remq active-loco locos))
        (send loco-select-menu delete (send loco-select-menu get-selection))
        (if (empty? locos)
          (begin (set-active-loco! #f)
                 (send loco-select-menu append "---"))
          (set-active-loco!
            (list-ref locos (send loco-select-menu get-selection))))))

    (define loco-control
      (new vertical-pane%
           (parent this)))

    (define speed-control
      (new horizontal-pane%
           (parent loco-control)))

    ; initialize slider to control the active loco's speed
    (define speed-slider
      (new slider%
           (label "Speed")
           (parent speed-control)
           (min-value 0)
           (max-value max-speed)
           (min-width 150)
           (style '(horizontal vertical-label))
           (init-value 0)
           (enabled #f)
           (callback
             (lambda (slider evt)
               (when active-loco
                 (active-loco-update!)
                 (send nmbs set-loco-speed active-loco
                       (send slider get-value)))))))

    (define route-choice
      (new choice%
           (label "Go to")
           (parent loco-control)
           (choices (map symbol->string d-block-ids))
           (vert-margin 0)
           (enabled #f)
           (callback
             (lambda (choice evt)
               (thread (lambda ()
                         (let ((idx (send choice get-selection)))
                           (when (and idx active-loco)
                             (send nmbs
                                   route
                                   active-loco
                                   (list-ref d-block-ids idx))))))))))

    (define reverse-button
      (new button%
           (label "Reverse")
           (parent speed-control)
           (enabled #t)
           (callback
             (lambda (btn evt)
               (when active-loco
                 (send nmbs change-loco-direction active-loco))))))

    (define remove-button
      (new button%
           (label "Remove")
           (parent loco-control)
           (enabled #t)
           (callback
             (lambda (btn evt)
               (when active-loco
                 (send nmbs remove-loco active-loco)
                 (remove-loco-from-menu!))))))))


;; main panel to control the railway's switches
(define switch-panel%
  (class vertical-panel%
    (init-field nmbs)
    (super-new (enabled #t)
               (style '(border))
               (alignment '(center top)))
    (define buttons (make-hash))
    (define (mk-label id (pos (send nmbs get-switch-position id)))
      (format "~a: ~a" id pos))
    (define (switch-changed id position)
      (send (hash-ref buttons id) set-label (mk-label id position)))
    (send nmbs add-switch-listener switch-changed)
    (for ((id (in-list (sort (send nmbs get-switch-ids) id<?))))
      (hash-set! buttons
                 id
                 (new button%
                      (label (mk-label id))
                      (parent this)
                      (enabled #t)
                      (vert-margin 0)
                      (horiz-margin 0)
                      (callback
                        (lambda (button evt)
                          (let ((pos (send nmbs get-switch-position id)))
                            (if (= pos 1)
                              (send nmbs set-switch-position id 2)
                              (send nmbs set-switch-position id 1))))))))))

(define db-light%
  (class object%
    (init-field name bmp-dc)
    (field (status 'green))
    (super-new)
    (define bmp (make-bitmap 120 20))
    (define char-h (send bmp-dc get-char-height))
    (define/public (get-bmp)
      bmp)
    (define/public (set-status new-status)
      (set! status new-status)
      (send bmp-dc set-bitmap bmp)
      (send bmp-dc set-background
            (case status
              ((red) (make-color 255 0 0))
              ((green) (make-color 0 255 0))
              ((orange) (make-color 255 200 0))))
      (send bmp-dc clear)
      (send bmp-dc draw-text name 8 1))))


(define d-block-panel%
  (class group-box-panel%
    (init-field nmbs d-block-ids)
    (super-new (label "Detection blocks")
               (enabled #t)
               (min-width 120)
               (stretchable-width #f)
               (alignment '(center top)))
    (define d-block-gfx
      (for/list ((db (in-list d-block-ids)))
        (make-object db-light% (symbol->string db) (new bitmap-dc%))))
    (define dbs
      (for/hash ((db (in-list d-block-ids))
                 (l (in-list d-block-gfx)))
        (send l set-status 'green)
        (values db l)))
    (define canvas
      (new canvas% (parent this)
           (paint-callback
             (lambda (canvas dc)
               (for ((light (in-list d-block-gfx))
                     (i (in-range 3 1000 23)))
                 (send dc draw-bitmap (send light get-bmp) 0 i))))))
    (define (change-db id status)
      (send (hash-ref dbs id) set-status status)
      (send canvas refresh))
    (send nmbs add-d-block-listener change-db)))


;; function used to sort identity symbols in a logical way
;: first on the first character defining their type
;; then on any numerical value in the remainder of the symbol
(define (id<? a b)
  (define (get-nums id)
    (string->number
      (list->string
        (filter char-numeric?
                (string->list
                  (symbol->string id))))))
  (define (get-first-char id)
    (string-ref (symbol->string id) 0))
  (let ((a-nums (get-nums a))
        (b-nums (get-nums b)))
    (if (and a b (char=? (get-first-char a) (get-first-char b)))
      (< a-nums b-nums)
      (symbol<? a b))))

