
;=====================================================================================================
; The Tower of Hanoi
; By Jacob J. A. Koot
;=====================================================================================================
;
; A GUI to play the game of The Tower of Hanoi. It has buttons. A click on a button initiates an
; action. Modal dialogs are used to exchange information between the GUI and the user. Moves can
; be made manually with the mouse but also automatically by the GUI. Use module "GUI.scrbl" to
; make documentation for the user.
;
;=====================================================================================================

#lang racket/base

(provide
  tower-of-hanoi
  idle-limit)

(require  
  graphics/graphics
  racket/gui/base
  (only-in racket
    infinite?
    make-list
    processor-count
    range
    ~r
    (define        DEFINE       )
    (define-values DEFINE-VALUES))
  (for-syntax
    racket/base
    racket/syntax
    syntax/transformer))
    
;=====================================================================================================
; Syntaxes define and define-values are redefined such as to produce immutable variables. For mutable
; variables DEFINE and DEFINE-VALUES must be used, which are imported from racket/base as synonyms of
; their original versions. The value of an immutable variable can be mutable, for example a mutable
; vector or a structure with mutable fields. (The phrase "immutable variable" sounds like a
; contradictio in terminis.(☺))

(define-syntax (define stx)
  (define (extract-id head)
    (syntax-case head ()
      ((head arg ...)
       (if (identifier? #'head) #'head
         (extract-id #'head)))
      (_ (raise-syntax-error 'define "not an identifier" stx head))))
  (syntax-case stx ()
    ((_ id value)
     (identifier? #'id)
     (with-syntax ((var (generate-temporary (syntax-e #'id))))
       #'(begin
           (DEFINE var value)
           (define-syntax id (make-variable-like-transformer #'var)))))
    ((_ head body ...)
     (with-syntax ((id (extract-id #'head)))
       #'(define id (let () (DEFINE head body ...) id))))))
  
(define-syntax (define-values stx)
  (define (check-identifiers ids)
    (cond
      ((null? ids) #t)
      ((identifier? (car ids)) (check-identifiers (cdr ids)))
      (else (raise-syntax-error 'define-values "not an identifier" stx (car ids)))))
  (define (check-duplicates ids)
    (define dupid (check-duplicate-identifier ids))
    (when dupid (raise-syntax-error 'define-values "duplicate identifier" stx dupid)))
  (syntax-case stx ()
    ((_ (id ...) expr)
     (let ((ids (syntax->list #'(id ...))))
       (check-identifiers ids)
       (check-duplicates ids)
       (with-syntax (((var ...) (generate-temporaries #'(id ...))))
         #'(begin
             (DEFINE-VALUES (var ...) expr)
             (define-syntax id (make-variable-like-transformer #'var)) ...))))))

;=====================================================================================================

(define-syntax-rule
  (in-reversed-range n)
  (in-range (sub1 n) -1 -1))

; Define values with in addition a list of these values.

(define-syntax-rule
  (define-values-with-list-of-values the-list (var value) ...)
  (begin
    (define-values (var ...) (values value ...))
    (define the-list (list var ...))))

; Defines values accumulatively, each one, the first one excepted, made from the previous one by a
; make-next procedure. Procedure make-next is not applied to last-id.

(define-syntax-rule
  (define-values-accumulative (id ... last-id) first make-next)
  (define-values (id ... last-id)
    (apply values
      (let
        ((first-val first))
        (for/fold ((val first-val) (vals (list first-val)) #:result (reverse vals))
          ((index (in-list '(id ...))))
          (let ((next-val (make-next val)))
            (values next-val (cons next-val vals))))))))

;=====================================================================================================
; Main procedure.

(define (tower-of-hanoi)
  (dynamic-wind
    void
    GUI
    close))

(define (GUI)
  (let/cc cc
    (initialize cc)
    (parameterize ((current-custodian top-custodian)) (main))))

(define (main) (action) (main))

(define (action) (dispatch (mouse-click-posn (time-out (get-mouse-click viewport)))))

(define (close)
  ; In order to make a user break work properly some care is required for termination of threads made
  ; by action compute. Both the line with 'kill-thread' and that with 'custodian-shutdown-all' are
  ; required and 'kill-thread' must precede 'custodian-shutdown-all'. Without this precaution DrRacket
  ; won't halt, although using less than 1% of CPU. It paralyzes and will not respond to any mouse-
  ; click or key press. 
  (for-each kill-thread running-threads)
  (custodian-shutdown-all top-custodian)
  (close-viewport viewport)
  (close-graphics))

;=====================================================================================================
; Internal state. The following variables can be mutated while playing. They are initialized or
; reinitialized by procedure initialize. Reinitialization is necessary when the GUI is called more
; than once from the same instantiation. Variables escape, viewport and top-custodian are not mutated
; again after they have been initialized or reinitialized. The viewport can not yet be assigned
; because this needs graphics to be open. Graphics is opened by procedure initialize which also will
; open and assign the viewport. The top-custodian is shut down during termination. A shut down
; custodian can no longer be used. Therefore the top-custodian must be reinitialized too. Variable
; last-compute is initialized but not reinitialized. The last command given to action compute is
; memorized and can be edited during subsequent calls.

(DEFINE allow-intro   'mutable)
(DEFINE clock         'mutable)
(DEFINE delay         'mutable)
(DEFINE disk-distr    'mutable)
(DEFINE height        'mutable)
(DEFINE manual-count  'mutable)
(DEFINE move-count    'mutable)
(DEFINE str-msg       'mutable)
(DEFINE last-compute  ""      ) ; Initialized here. Not reinitialized by procedure initialize.
(DEFINE escape        'delayed)
(DEFINE top-custodian 'delayed)
(DEFINE viewport      'delayed)

;=====================================================================================================
; Initialize or reinitialize mutable variables and buttons with content.
; Open graphics and the viewport. Draw the GUI.

(define (initialize cc)
  ; Initialize or reinitialize mutable variables of the internal state.
  (set! allow-intro                 #t)
  (set! clock                        0)
  (set! delay                    click)
  (set! escape                      cc)
  (set! height              max-height)
  (set! manual-count                 0)
  (set! move-count                   0)
  (set! str-msg                     "")
  (set! top-custodian (make-custodian))
  ; Open graphics and the viewport.
  (open-graphics)
  (set! viewport (open-viewport "Tower of Hanoi" vp-width vp-height))
  ; Initialize or reinitialize buttons with content.
  (button-height 'put-content height      )
  (button-mode   'put-content manual      )
  (button-delay  'put-content delay       )
  (button-idle   'put-content (idle-limit))
  ; Draw the buttons.
  (for ((button (in-list all-buttons)))
    (draw-button button)
    (when (button2? button) (draw-button-content button (button2-content button))))
  ; Disable button cancel.
  (button-cancel 'disable)
  ; Draw the girder on which the pegs will be mounted.
  (draw-girder)
  ; Procedure action-reset draws the pegs and places all disks at the left peg.
  ; Also initializes or reinitializes variable disk-distr.
  (action-reset))

;=====================================================================================================
; When the GUI is waiting for a mouse-click or a response to a modal dialog but the user does not
; click or answer within a certain time, the GUI aborts. The limit is hold in parameter idle-limit.

(define default-idle-minutes 10)
(define max-idle-minutes  10080) ; A full week (7×24×60 = 10080)
(define min-idle-minutes      1)

(define idle-limit
  (make-parameter
    default-idle-minutes
    (λ (time) ; Minutes.
      (cond
        ((and (exact-positive-integer? time) (<= time max-idle-minutes)) time)
        (else
          (raise-user-error '|Parameter idle-limit|
            "\n  Exact positive integer ~s<=time<=~s wanted.\n  Given ~s"
            min-idle-minutes  max-idle-minutes time))))
    'parameter-idle-limit))

;=====================================================================================================
; Timing out after exceeding the idle-limit.

(define-syntax (time-out stx)
  (syntax-case stx ()
    ((_ #f expr ...) #'(time-out-proc #f (λ () expr ...)))
    ((_ #t expr ...) #'(time-out-proc #t (λ () expr ...)))
    ((_    expr ...) #'(time-out-proc #f (λ () expr ...)))))

(define not-finished (string->uninterned-symbol "not-finished"))

(define (time-out-proc dialog? thunk)
  (when dialog?
    ((draw-string viewport) posn-warn1 str-warn1 red)
    ((draw-string viewport) posn-warn2 str-warn2 red))
  (define result-box (box not-finished))
  (define custodian (make-custodian top-custodian))
  (define (handle-thread thread)
    (sync/timeout (* (idle-limit) 60) thread) ; Minutes to seconds.
    (kill-thread thread)
    (custodian-shutdown-all custodian)
    (define result (unbox result-box))
    (cond
      ((eq? result not-finished)              ; Maximum idle time exceeded. Abort.
       (time-out-abort))
      (else                                   ; Answer received within maximum idle time.
        (when dialog?                         ; Clear warning when applicable.
          ((clear-string viewport) posn-warn1 str-warn1)
          ((clear-string viewport) posn-warn2 str-warn2))
        (apply values result))))              ; Return the result, possibly a multiple value.
  (parameterize ((current-eventspace (make-eventspace)))
    (handle-thread
      (thread
        (λ () (set-box! result-box (call-with-values thunk list)))
        #:pool 'own))))

(define (time-out-abort)
  (define limit (idle-limit))
  (fprintf (current-error-port)
    "\nTower of Hanoi\n  ~
       No activity during ~a. Game aborted.\n  ~
       Use parameter idle-limit to increase the allowed\n  ~
       idle time or use the Idle limit button.\n\n"
    (if (= limit 1) "1 minute" (format "~s minutes" limit)))
  (custodian-shutdown-all top-custodian)
  (escape))

;=====================================================================================================
; Variables that can and some of which must be defined in early stage because they are referenced in
; early stage and a variable cannot be referenced before it is defined.

(define base-size        20                                        )
(define border           (* 3 base-size)                           )
(define max-height       10                                        )
(define disk-height      base-size                                 )
(define max-tower-height (* max-height disk-height)                )
(define min-disk-width   (* 3 base-size)                           )
(define disk-width-incr  base-size                                 )
(define (disk-width d)   (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width   (disk-width (sub1 max-height))            )
(define peg-top          (* 2 base-size)                           )
(define peg-width        4                                         )
(define click            'click                                    )
(define str-offset       4                                         )
(define 2*str-offset     (* 2 str-offset)                          )
(define str-click        (symbol->string click)                    )
(define str-warn1        " A dialog is waiting."                   )
(define str-warn2        " Look for it when you don't see it."     )
(define manual           'manual                                   )
(define white            (make-rgb 1.0 1.0 1.0)                    )
(define black            (make-rgb 0.0 0.0 0.0)                    )
(define gray             (make-rgb 0.6 0.6 0.6)                    )
(define red              (make-rgb 1.0 0.0 0.0)                    )
(define green            (make-rgb 0.0 0.8 0.0)                    )
(define blue             (make-rgb 0.0 0.0 1.0)                    )

(define-values-with-list-of-values button-names
  (str-height     " Height "    )
  (str-mode       " Mode "      )
  (str-delay      " Delay "     )
  (str-idle-limit " Idle limit ")
  (str-short      " short "     )
  (str-long       " long "      )
  (str-circular   " circular "  )
  (str-compute    " Compute "   ))
  
;=====================================================================================================
; Buttons. They have procedure property. A button contains its name, its position, its region and a
; boolean indicating whether or not it is enabled. Some buttons contain a content too. When called
; with action in-button? procedures button1 and button2 receive a posn for argument pos cq arg.

(define (proc-button1 button action (pos #f)) ; For the procedure property of buttons without content.
  (case action
    ((in-button?)
     (in-region? pos (button1-region button)))
    ((enabled?)
     (button1-enabled?               button))
    ((disable)
     (set-button1-enabled?!          button #f)
     (draw-disabled-button           button))
    ((enable)
     (set-button1-enabled?!          button #t)
     (draw-button                    button))))

(define (proc-button2 button action (arg #f)) ; For the procedure property of buttons with content.
  (case action
    ((get-content)
     (button2-content      button           ))
    ((put-content)
     (set-button2-content! button        arg)
     (draw-button-content  button        arg))
    (else (proc-button1    button action arg))))

(struct button1 ((enabled? #:mutable) region pos name) ; Without content.
  #:property prop:procedure proc-button1
  #:constructor-name make-button1)

(struct button2 button1 ((content #:mutable))          ; With content.
  #:property prop:procedure proc-button2
  #:omit-define-syntaxes
  #:constructor-name make-button2)

(define (make-button name position (content #f))
  ; Constructor make-button is called either without content or with a true content, never false.
  ; Hence, when content is #f, a button without content must be made, else one with content.
  (let
    ((region (make-region position button-width button-hght))
     (str-name (symbol->string name))
     (enabled #t))
    (cond
      (content (make-button2 enabled region position str-name content))
      (else    (make-button1 enabled region position str-name     )))))

; Buttons can be disabled and enabled.

(define (enable/disable-buttons buttons enable/disable)
  (for ((button (in-list buttons))) (button enable/disable)))

;=====================================================================================================
; Dispatch of mouse-clicks.

(define-syntax (dispatch-button stx)
  (syntax-case stx (else)
    ((_ pos (button action ...) ... (else else-action ...))
     #'(let ((p pos))
         (cond
           ((button 'in-button? p) action ...) ...
           (else else-action ...))))
    ((_ pos (button action ...) ...)
     #'(let ((p pos))
         (cond
           ((button 'in-button? p) action ...) ...)))))

(define (dispatch pos)
  (dispatch-button pos
    (button-height  (action-height  ))
    (button-mode    (action-mode    ))
    (button-delay   (action-delay   ))
    (button-idle    (action-idle    ))
    (button-reset   (action-reset   ))
    (button-setup   (action-setup   ))
    (button-peg0    (action-manual 0))
    (button-peg1    (action-manual 1))
    (button-peg2    (action-manual 2))
    (button-compute (action-compute ))
    (button-quit    (action-quit    ))
    (else
      (define p (dispatch-peg pos))
      (when   p (action-manual p)))))

(define (dispatch-peg pos)
  (cond
    ((in-region? pos region-peg0) 0)
    ((in-region? pos region-peg1) 1)
    ((in-region? pos region-peg2) 2)
    (else #f)))

;=====================================================================================================
; Procedures drawing buttons and their contents.

(define (draw-button button)
  (define pos  (button1-pos  button))
  (define name (button1-name button))
  (define x (posn-x pos))
  (define y (posn-y pos))
  ((draw-solid-rectangle viewport) pos button-width button-hght blue)
  ((draw-string viewport)
   (make-posn (+ x 2*str-offset) (+ y button-hght (- 2*str-offset))) name white))

(define (draw-disabled-button button)
  (define pos  (button1-pos   button))
  (define name (button1-name  button))
  (define x (posn-x pos))
  (define y (posn-y pos))
  ((clear-solid-rectangle viewport) pos button-width button-hght)
  ((draw-rectangle        viewport) pos button-width button-hght blue)
  ((draw-string viewport)
   (make-posn (+ x str-offset) (+ y button-hght (- 2*str-offset))) name blue))

(define (draw-button-content button value)
  (define str (if (string? value) value (format "~a" value)))
  (define pos (button1-pos button))
  (define x (posn-x pos))
  (define y (+ (posn-y pos) button-hght))
  ((clear-solid-rectangle viewport) (make-posn x y) button-width button-hght)
  ((draw-rectangle        viewport) (make-posn x y) button-width button-hght blue)
  ((draw-string viewport)
   (make-posn (+ x 2*str-offset) (+ y button-hght (- 2*str-offset))) str blue))

;=====================================================================================================
; Procedures drawing pegs, disks and the girder.

(define (draw-pegs)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle viewport)
     (make-posn (peg-x p) peg-y)
     peg-width peg-height green)))

(define (remove-all-disks)
  ((clear-solid-rectangle viewport)
   (make-posn (+ base-size border) (- vp-height border base-size max-tower-height))
   (+ (* 3 max-disk-width) (* 2 border))
   max-tower-height)
  (draw-pegs))

(define (draw-disk d h p (color black))
  (define width (disk-width d))
  (define x (- (+ (peg-x p) (quotient peg-width 2)) (quotient width 2)))
  (define y (- vp-height border base-size (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle viewport) pos width disk-height color)
  ((draw-rectangle viewport) pos width disk-height white)
  ((draw-string viewport)
   (make-posn (- (peg-x p) 2) (+ y base-size -3))
   (format "~s" d) white))

(define (remove-disk d h p)
  (define width (disk-width d))
  (define center (+ (peg-x p) (quotient peg-width 2)))
  (define x (- center (quotient width 2)))
  (define y (- vp-height border base-size (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((clear-solid-rectangle viewport) pos width disk-height)
  ((draw-solid-rectangle viewport)
   (make-posn (- center (quotient peg-width 2)) y) peg-width disk-height green))

(define (mark-disk d h p) (draw-disk d h p red))

(define (draw-girder)
  ((draw-solid-rectangle viewport) posn-girder (- vp-width (* 2 border)) base-size gray)
  (for ((p (in-range 0 3)))
    (define str (format "Peg ~s" p))
    (define size (car ((get-string-size viewport) str)))
    ((draw-string viewport)
     (posn-add (make-posn (peg-x p) (- vp-height border 3))
       (- (quotient size 2))
       (- (quotient str-offset 2)))
     str white)))

;=====================================================================================================
; Computation of the dimensions of buttons. These depend on the sizes of the texts to be put into
; the buttons. The dimensions are determined by procedure get-string-size which requires a viewport
; or pixmap. A temporary pixmap is used. This requires graphics, which is temporarily opened too.
; Both will be closed after finishing the computation.

(define-values (button-width button-hght)
  (let ()
    (open-graphics)
    (define pixmap (open-pixmap "string-sizes" 1000 500))
    (dynamic-wind
      void
      (λ ()
        (for/fold ((width 0) (height 0) #:result (values width height))
          ((string (in-list button-names)))
          (let ((dimensions ((get-string-size pixmap) string)))
            (values
              (max width  (+ 2*str-offset (ceiling (inexact->exact (car  dimensions)))))
              (max height (+ 2*str-offset (ceiling (inexact->exact (cadr dimensions)))))))))
      (λ () (close-viewport pixmap) (close-graphics)))))

;=====================================================================================================
; Lay out of the GUI. Positions and dimensions of all objects to be drawn in the GUI.

(define (posn-add pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
(define button-w+b  (+ button-width base-size))
(define peg-y  (* 2 (+ border button-hght)))
(define peg-height  (+ peg-top max-tower-height))
(define vp-width    (+ (* 3 max-disk-width)   (* 2 base-size ) (* 4 border)))
(define vp-height   (+ (* 2 button-hght) (* 3 border) peg-height base-size))

(define (peg-x p)
  (+ border
    base-size
    (* p (+ border max-disk-width))
    (quotient (- max-disk-width peg-width) 2)))

(define-values-accumulative
  (posn-height
    posn-mode
    posn-delay
    posn-idle
    posn-reset
    posn-setup
    posn-quit
    posn-cancel
    posn-peg0
    posn-peg1
    posn-peg2
    posn-compute)
  (make-posn border border)
  (λ (pos) (posn-add pos button-w+b 0)))

(define posn-move-count (posn-add  posn-idle button-w+b (- (* 2  button-hght) str-offset)))
(define posn-warn1      (posn-add  posn-compute button-w+b (- (+ button-hght  str-offset) base-size)))
(define posn-warn2      (posn-add  posn-warn1 0 base-size))
(define posn-warn3      (posn-add  posn-warn2 0 base-size))
(define posn-girder     (make-posn border (- vp-height border base-size)))

;=====================================================================================================
; A region records the position and dimensions of objects whose clicks must be dispatched.

(struct region (pos width height)
  #:omit-define-syntaxes
  #:constructor-name make-region)

(define (in-region? pos region)
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define x-min (posn-x  (region-pos    region)))
  (define y-min (posn-y  (region-pos    region)))
  (define x-max (+ x-min (region-width  region)))
  (define y-max (+ y-min (region-height region)))
  (and (<= x-min x x-max) (<= y-min y y-max)))

(define-values (region-peg2 region-peg1 region-peg0)
  (apply values
    (for/fold
      ((pos
         (make-posn
           (+ border base-size)
           (- vp-height border base-size peg-height)))
       (regions '()) #:result regions)
      ((n (in-range 3)))
      (values
        (posn-add pos (+ max-disk-width border) 0)
        (cons (make-region pos max-disk-width peg-height) regions)))))

;=====================================================================================================
; Now define the buttons. Contents are mutable, but the initial contents always are the same and
; reinitialized by procedure initialize. Button-idle is an exception. Its content is taken from
; parameter idle-limit.

(define-values-with-list-of-values all-buttons
  (button-height  (make-button 'Height       posn-height max-height  ))  ; With content.
  (button-mode    (make-button 'Mode         posn-mode   manual      ))  ; With content.
  (button-delay   (make-button 'Delay        posn-delay  click       ))  ; With content.
  (button-idle    (make-button '|Idle limit| posn-idle   (idle-limit)))  ; With content.
  (button-reset   (make-button 'Reset        posn-reset              ))  ; Without content.
  (button-setup   (make-button 'Setup        posn-setup              ))  ; Without content.
  (button-quit    (make-button 'Quit         posn-quit               ))  ; Without content.
  (button-cancel  (make-button 'Cancel       posn-cancel             ))  ; Without content.
  (button-peg0    (make-button '|Peg 0|      posn-peg0               ))  ; Without content.
  (button-peg1    (make-button '|Peg 1|      posn-peg1               ))  ; Without content.
  (button-peg2    (make-button '|Peg 2|      posn-peg2               ))  ; Without content.
  (button-compute (make-button 'Compute      posn-compute            ))) ; Without content.

;=====================================================================================================
; When validating a modal dialog that returns a string, data must be read from the string, possibly
; followed by some computation. We don't want the GUI to crash when the user provides wrong or even
; unreadable data. The validator simply must reject the answer given by the user. Therefore the
; following handler.

(define-syntax-rule
  (catch-exn expr ...)
  (with-handlers ((exn:fail? catch-exn:fail)) expr ...))

(define (catch-exn:fail e) #f)

;=====================================================================================================
; Action manual.

(define (action-manual from-peg)
  (define from-peg-disks (vector-ref disk-distr from-peg))
  (unless (null? from-peg-disks) ; Ignore click if the selected peg is empty.
    (define d (car from-peg-disks))
    (when ; Act only if the selected disk can be moved, else do nothing.
      (or
        (< d (size-of-top-disk (modulo (+ 1 from-peg) 3)))
        (< d (size-of-top-disk (modulo (+ 2 from-peg) 3))))
      (define h (sub1 (length from-peg-disks)))
      (mark-disk d h from-peg)
      (action-manual1 d h from-peg))))

(define (action-manual1 d h from-peg)             ; Let's see at which peg to put disk d.
  (button-cancel 'enable)
  (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
  (dispatch-button pos
    (button-peg0 (action-manual2 d h from-peg 0)) ; Peg 0 selected.
    (button-peg1 (action-manual2 d h from-peg 1)) ; Peg 1 selected.
    (button-peg2 (action-manual2 d h from-peg 2)) ; Peg 2 selected.
    (button-cancel                                ; Move canceled. Unmark the selected disk.
      (draw-disk d h from-peg)
      (button-cancel 'disable)
      (reset-manual-count))
    (else                                         ; Disk not selected by means of a peg button, nor
      (define dest-peg (dispatch-peg pos))        ; canceled. May be selected by a click near a peg.
      (cond                                       ; Use manual2 to move the disk to peg dest.
        (dest-peg (action-manual2 d h from-peg dest-peg)) ; Yes, destination peg selected.
        (else
          (button-cancel 'disable)                ; Something else than a peg selected.
          (draw-disk d h from-peg)                ; Unmark the selected disk and
          (dispatch pos))))))                     ; dispatch the mouse-click

(define (action-manual2 d h from-peg to-peg)
  (cond
    ((= to-peg from-peg) (draw-disk d h from-peg))
    (else
      (define dest-peg-list-of-disks (vector-ref disk-distr to-peg))
      (cond
        ((< d (size-of-top-disk to-peg)) ; Is the move allowed?
         (remove-disk d h from-peg)      ; Yes it is. Move it.
         (vector-set! disk-distr from-peg (cdr (vector-ref disk-distr from-peg)))
         (vector-set! disk-distr to-peg (cons d dest-peg-list-of-disks))
         (draw-disk d (length dest-peg-list-of-disks) to-peg)
         (increment-manual-count))
        (else                            ; The move is not allowed. Unmark the
          (draw-disk d h from-peg)))))   ; selected disk and ignore the mouse-click
  (button-cancel 'disable))

(define (size-of-top-disk p)             ; In fact the disk ordinal is returned, not its size.
  (define peg (vector-ref disk-distr p)) ; The order of sizes is the same as that of the ordinals.
  (if (null? peg)
    max-height  ; Size of a virtual top disk at an empty peg. Greater than all real disks.
    (car peg))) ; Size of the top disk at a non-empty peg.

;=====================================================================================================
; While moving disks manually or with delay click, the number of moves is shown.

(define (increment-manual-count)
  (set! manual-count (add1 manual-count))
  (clear-manual-count)
  (draw-manual-count))

(define (clear-manual-count) ((clear-string viewport) posn-move-count str-msg))

(define (draw-manual-count)
  (when (> manual-count 0)
    (clear-manual-count)
    (set! str-msg (format "Manual moves ~s" manual-count))
    ((draw-string viewport) posn-move-count str-msg)))

(define (reset-manual-count) (clear-manual-count) (set! manual-count 0))

;=====================================================================================================
; Action height.

(define (validate-height str)
  (catch-exn
    (define h (read (open-input-string str)))
    (and (exact-nonnegative-integer? h) (<= 1 h max-height))))

(define (action-height)
  (define str
    (time-out #t
      (get-text-from-user
        str-height
        (format
          "How many disks do you want?\n~
           At least one, at most ten.")
        #f
        "10"
        '(disallow-invalid)
        #:validate validate-height)))
  (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
  (when str
    (define h (read (open-input-string str)))
    (set! height h)
    (button-height 'put-content h)
    (reset-manual-count)
    (action-reset)))

;=====================================================================================================
; Action mode. When finishing mode short, long or circular, notify the user. Disable/enable all
; buttons, reset, quit and cancel excepted. The latter allow aborting the action.

(define (finish who)
  (time-out #t (message-box who "\n\n\nfinished\n\n\n" #f '(ok no-icon)))
  (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
  (prepare/finish-action-mode 'enable)
  (button-cancel 'disable)
  ((clear-string viewport) posn-move-count str-msg)
  (button-mode 'put-content manual))

(define buttons-for-action-mode   ; All buttons, reset, quit and cancel excepted.
  (remove* (list button-reset button-quit button-cancel) all-buttons))

(define (prepare/finish-action-mode enable/disable)
  (enable/disable-buttons buttons-for-action-mode enable/disable))

(define (action-mode)
  (prepare/finish-action-mode 'disable)
  (define choice
    (time-out #t
      (get-choices-from-user
        str-mode
        "Select a mode\nCancel in order to remain in manual mode."
        (list str-short str-long str-circular)
        #f
        '()
        '(single))))
  (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
  (when choice
    (button-cancel 'enable)
    (reset-manual-count)
    (define ch (car choice))
    (define action (vector-ref (vector short long circular) ch))
    (define mode   (vector-ref #(      short long circular) ch))
    (button-mode 'put-content mode)
    (unless (eq? mode 'short) (action-reset))
    (action)
    (finish (symbol->string mode)))
  (button-cancel 'disable)
  (prepare/finish-action-mode 'enable))

;=====================================================================================================
; Action short mode.

(define (short)
  (reset-time-and-move-counter)
  (let/cc cc
    ; The exit allows procedure move-disk to quit from the action.
    (define (exit) (clear-msg) (cc))
    (define distr
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (define (short distr dest)
      (cond
        ((null? distr))
        ((= (car distr) dest) (short (cdr distr) dest))
        (else
          (define new-conf (- 3 (car distr) dest))
          (short (cdr distr) new-conf)
          (move-disk (car distr) dest exit)
          (short (make-list (length (cdr distr)) new-conf) dest))))
    (short distr 2)))

;=====================================================================================================
; Action long mode.

(define (long)
  (action-reset)
  (reset-time-and-move-counter)
  (let/cc cc
    ; The exit allows procedure move-disk to stop the action.
    (define (exit) (clear-msg) (cc))
    (define p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (define (long conf dest)
      (cond
        ((null? conf))
        (else
          (define h (sub1 (length conf)))
          (define third (- 3 (car conf) dest))
          (long (cdr conf) dest)
          (move-disk (car conf) third exit)
          (long (make-list h dest) (car conf))
          (move-disk third dest exit)
          (long (make-list h (car conf)) dest))))
    (long p-list 2)))

;=====================================================================================================
; Action circular mode.

(define (circular)
  (action-reset)
  (reset-time-and-move-counter)
  (let/cc cc
    ; The exit allows procedure move-disk to stop the action.
    (define (exit) (clear-msg) (cc))
    (define (longest-circular-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path h-1 f r)
        (move-disk f t exit)
        (longest-path h-1 r f)
        (move-disk t r exit)
        (longest-path h-1 f t)
        (move-disk r f exit)
        (finish-path h-1 t f)))
    (define (longest-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-path h-1 f t)
        (move-disk f r exit)
        (longest-path h-1 t f)
        (move-disk r t exit)
        (longest-path h-1 f t)))
    (define (start-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (start-path h-1 f r)
        (move-disk f t exit)
        (longest-path h-1 r t)))
    (define (finish-path h f t)
      (unless (zero? h)
        (define h-1 (sub1 h))
        (define r (- 3 f t))
        (longest-path h-1 f r)
        (move-disk f t exit)
        (finish-path h-1 r t)))
    (longest-circular-path height 0 1)))

;=====================================================================================================
; Keeping record of the number of moves and the real CPU time used so far for them.

(define (reset-time-and-move-counter)
  (clear-msg)
  (set! clock (current-inexact-milliseconds))
  (set! move-count -1)
  (set! str-msg "")
  (draw-count-msg))

(define (clear-msg) ((clear-string viewport) posn-move-count str-msg))

(define (draw-count-msg)
  (clear-msg)
  (set! move-count (add1 move-count))
  (set! str-msg
    (if (eq? delay click)
      (format "Move count: ~s" move-count)
      (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
  ((draw-string viewport) posn-move-count str-msg))

(define (watch-clock)
  (~r #:precision (list '= 3) (/ (- (current-inexact-milliseconds) clock) 1000)))

;=====================================================================================================
; Actions short, long and circular mode use procedure move-disk. It allows abort from the action by
; means of buttons reset and cancel. For this purpose procedure move-disk receives a continuation of
; the calling action. If the delay is not click it uses procedure doze in order to make the moves at
; the desired rate.

(define (move-disk f t exit)
  (define ff (vector-ref disk-distr f))
  (define tt (vector-ref disk-distr t))
  (unless (null? ff)
    (define d (car ff))
    (define move-to-be-made?
      (case delay
        ((click) (check-click #t exit))
        (else
          (define first-doze-time (min delay 1.5))
          (cond
            ((= move-count 0) (doze first-doze-time exit) (check-click #f exit))
            (else (doze delay exit) (check-click #f exit))))))
    (cond
      (move-to-be-made?
        (remove-disk d (sub1 (length ff)) f)
        (draw-disk d (length tt) t)
        (vector-set! disk-distr f (cdr ff))
        (vector-set! disk-distr t (cons d tt))
        (draw-count-msg))
      (else (move-disk f t exit)))))

(define (check-click click-required? exit)
  (define pos
    (if click-required?
      (time-out (get-mouse-click viewport))
      (ready-mouse-click viewport)))
  (define p (and pos (mouse-click-posn pos)))
  (when p
    (dispatch-button p
      (button-reset  (action-reset) (exit) #f)
      (button-cancel                (exit) #f)
      (button-quit   (action-quit )        #f)
      (else #t))))

(define (doze t exit) ; t in seconds. Like sleep, but catching reset, cancel and quit.
  (cond
    ((zero? delay) (doze-help exit))
    (else
      (define starting-time (current-inexact-milliseconds))
      (define finish-time (+ starting-time (* 1000 t)))
      (define sleeping-time (min 0.25 (/ delay 1.01))) ;Periodically check for reset, cancel and quit.
      (define (doze-loop)
        (when (< (current-inexact-milliseconds) finish-time)
          (sleep sleeping-time) (doze-help exit) (doze-loop)))
      (doze-loop))))

; Capture and process clicks on buttons reset, cancel and quit.

(define (doze-help exit)
  (define click (ready-mouse-click viewport))
  (when click
    (dispatch-button (mouse-click-posn click)
      (button-reset  (action-reset) (exit))
      (button-cancel                (exit))
      (button-quit   (action-quit )      ))))

;=====================================================================================================
; Action delay.

(define (action-delay)
  (define (validate-delay str)
    (and (<= 1 (string-length str) 6)
      (or
        (equal? str str-click)
        (catch-exn
          (define input (open-input-string str))
          (define delay (read input))
          (cond
            ((not (eof-object? (read input))) #f)
            ((infinite? delay) #f)
            ((and (real? delay) (>= delay 0)))
            (else #f))))))
  (define str
    (time-out #t
      (get-text-from-user
        str-delay
        (format
          "Enter a non-negative real number for the\n~
           approximate delay in seconds between moves\n~
           or leave the default 'click' as it is.\n~
           Do not enter more than 6 characters")
        #f	
        str-click	
        '(disallow-invalid) 	
        #:validate validate-delay)))
  (cond
    ((equal? str str-click)
     (set! delay click)
     (button-delay 'put-content click))
    ((not str))
    (else
      (define d (read (open-input-string str)))
      (set! delay d)
      (button-delay 'put-content d)))
  (viewport-flush-input viewport)) ; Ignore mouse-clicks made before a response on the dialog.

;=====================================================================================================
; Action idle limit.

(define (action-idle)
  (define (validate-delay str)
    (and (<= 1 (string-length str) 5)
      (catch-exn
        (define input (open-input-string str))
        (define idle-limit (read input))
        (and (exact-positive-integer? idle-limit)
          (<= min-idle-minutes idle-limit max-idle-minutes)))))
  (define str
    (time-out #t
      (get-text-from-user
        str-idle-limit
        (format
          "Enter an exact positive integer number not exceeding ~s\n~
           for the maximally allowed idle time in minutes.\n~
           Do not enter more than 5 characters."
          max-idle-minutes)
        #f	
        "10"	
        '(disallow-invalid) 	
        #:validate validate-delay)))
  (when str
    (define minutes (read (open-input-string str)))
    (idle-limit minutes)           ; Adapt parameter idle-limit too.
    (draw-button-content button-idle minutes))
  (viewport-flush-input viewport)) ; Ignore mouse-clicks made before a response on the dialog.

;=====================================================================================================
; Action reset.
; Remove all disks, redraw the pegs and draw the full tower at peg 0.

(define (action-reset)
  (set! disk-distr (vector (range height) '() '()))
  (remove-all-disks) ; Also draws the pegs that where hidden behind disks.
  (reset-manual-count)
  (for ((d (in-range height)) (h (in-reversed-range height)))
    (draw-disk d h 0)))

;=====================================================================================================
; Action setup.

(define buttons-for-action-setup
  (remove* (list button-reset button-cancel button-quit button-peg0 button-peg1 button-peg2)
    all-buttons))

(define (action-setup)
  (button-cancel 'enable)
  (reset-manual-count)
  (enable/disable-buttons buttons-for-action-setup 'disable)
  (set! str-msg "Setting up")
  (remove-all-disks)
  (set! disk-distr (make-vector 3 '()))
  ((draw-string viewport) posn-move-count str-msg red)
  (action-setup1 (reverse (range height)))
  (enable/disable-buttons buttons-for-action-setup 'enable)
  (button-cancel 'disable)
  (clear-msg))

(define (action-setup1 disks)
  (unless (null? disks)
    (define d (car disks))
    (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
    (dispatch-button pos
      (button-peg0   (action-setup2 d 0) (action-setup1 (cdr disks)))
      (button-peg1   (action-setup2 d 1) (action-setup1 (cdr disks)))
      (button-peg2   (action-setup2 d 2) (action-setup1 (cdr disks)))
      (button-reset  (clear-msg        ) (action-reset             ))
      (button-cancel (clear-msg        ) (action-reset             ))
      (button-quit   (action-quit      ) (action-setup1 (cdr disks)))
      (else
        (define p (dispatch-peg pos))
        (cond
          (p (action-setup2 d p) (action-setup1 (cdr disks)))
          (else (action-setup1 disks)))))))

(define (action-setup2 d p)
  (define peg (vector-ref disk-distr p))
  (vector-set! disk-distr p (cons d peg))
  (draw-disk d (length peg) p))

;=====================================================================================================
; Action quit.

(define (action-quit)
  (define answer
    (time-out #t
      (message-box/custom "Quit"
        "Ok to quit?"
        "yes"
        "cancel"
        #f
        #f
        '(caution default=2))))
  (when (eq? answer '1) (escape)))

;=====================================================================================================
; Action compute.

(DEFINE-VALUES (SLC h M m f t running-threads) (values #f #f #f #f #f #f '()))

(define-syntax (accept-cancel stx)
  (syntax-case stx ()
    ((_ expr) #'(accept-cancel-thunk (λ () expr)))))

(define (accept-cancel-thunk thunk)
  (define result-box (box #f))
  (define task
    (parameterize ((current-custodian (make-custodian top-custodian)))
      (thread (λ () (set-box! result-box (call-with-values thunk list))))))
  (let loop ()
    (sleep 1)
    (define click (ready-mouse-click viewport))
    (define results (unbox result-box))
    (cond
      ((unbox result-box))
      ((and click (button-cancel 'in-button? (mouse-click-posn click)))
       (for-each kill-thread running-threads)
       (kill-thread task)
       #f)
      (else (loop)))))

(define buttons-for-action-compute (remove button-cancel all-buttons))

(define namespace (make-base-namespace))
(define (catch-exn-for-compute e) (set! SLC 'wrong))
(define nr-of-disks-per-line 50)

(define (validate-compute str)
  (with-handlers ((exn:fail? catch-exn-for-compute))
    (define input (open-input-string str))
    (set! SLC (read input))
    (set! h   (read input)) (namespace-set-variable-value! 'h h #t namespace #f)
    (set! M   (read input))
    (set! m   (if (exact-positive-integer? M) M (eval M namespace)))
    (set! f   (read input))
    (set! t   (read input))
    (unless   (eof-object? (read input)) (raise (make-exn:fail 'ignored))))
  (or
    (and
      (member SLC '(S L C))
      (exact-integer? h)
      (exact-integer? m)
      (exact-integer? f)
      (exact-integer? t)
      (<= 0 f 2)
      (<= 0 t 2)
      (not (= f t))
      (let ((expt3h (expt 3 h)))
        (< 0 m (case SLC ((S) (expt 2 h)) ((L) expt3h) ((C) (add1 expt3h))))))
    (begin (set!-values (SLC h M m f t) (values 'wrong #f #f #f #f #f)))))

(define (action-compute)
  (define-values (ok answer)
    (cond
      (allow-intro
        (time-out #t
          (message+check-box str-compute
            (format
              "Computation of move m:\n  ~
                 which disk is moved,\n  ~
                 from which peg it is taken,\n  ~
                 onto which peg it put\n  ~
                 and the resulting distribution of disks\n\n~
               You will be asked for the following details:\n\n  ~
                 mode: capital letter: S for short, L for long and C for circular.\n  ~
                 height: number of disks (can be greater than 10).\n  ~
                 move: move number, starting from 1.\n  ~
                 from: starting peg 0, 1 or 2.\n  ~
                 onto: destination-peg 0, 1 or 2, but t≠f.\n\n~
               The move can be any expression for a positive exact integer\n~
               number not greater than allowed for the mode and height.\n~
               In the expression letter h can be used for the height\n\n  ~
                 For mode S: (<= 1 move (sub1 (expt 2 height)))\n  ~
                 For mode L: (<= 1 move (sub1 (expt 3 height)))\n  ~
                 For mode C: (<= 1 move (expt 3 height))")
            " Do not show this message next time."
            #f
            '(ok-cancel no-icon))))
      (else (values 'ok #f))))
  (when answer (set! allow-intro #f))
  (when (eq? ok 'ok) (compute-help #t))
  (set! running-threads '())
  (enable/disable-buttons buttons-for-action-compute 'enable)
  (button-cancel 'disable)
  (set!-values (SLC h M m f t) (values '#f #f #f #f #f #f)))

(define (compute-help first?)
  (define str
    (time-out #t
      (get-text-from-user str-compute
        (string-append
          (if first? "" "Wrong data, try again editing it or cancel.\n")
          "Give mode, height, move, from-disk and onto-disk\n"
          "The move can be an expression in which h is the height.\n")
        #f
        last-compute)))
  (viewport-flush-input viewport)
  (when str
    (set! last-compute str)
    (validate-compute str)
    (cond
      ((eq? SLC 'wrong) (compute-help #f))
      (else
        (define c-str (string-append "Computing: " str))
        (draw-compute-warning c-str)
        (button-cancel 'enable)
        (enable/disable-buttons buttons-for-action-compute 'disable)
        (define result
          (accept-cancel
            ((case SLC
               ((S) compute-short)
               ((L) compute-long)
               ((C) compute-circular))
             h m f t)))
        (clear-compute-warning c-str)
        (when result
          (define-values (d ff tt distr) (apply values result))
          (define distr-str
            (apply string-append
              (for/fold
                ((result '()) #:result (reverse result))
                ((p (in-list distr)) (n (in-cycle (in-range 0 nr-of-disks-per-line))))
                (if (= n (sub1 nr-of-disks-per-line))
                  (cons "\n" (cons (format "~s" p) result))
                  (cons (format "~s" p) result)))))
          (time-out #t
            (message-box str-compute
              (if (eq? SLC 'C)
                (format
                  "Results for move ~a of path C with ~s disks from peg ~s ~
                   via peg ~s and peg ~s back to peg ~s\n\n~
                   Disk ~s from peg ~s to peg ~s.\n~
                   Resulting distribution:\n~
                   Positions of disks in order of increasing size:\n~a\n"
                  M h f t (- 3 f t) f d ff tt distr-str)
                (format
                  "Results for move ~a of path ~a from peg ~s to peg ~s with ~s disks.\n\n~
                   Disk ~s from peg ~s onto peg ~s.\n~
                   Resulting distribution:\n~
                   Positions of disks in order of increasing size:\n~a\n"
                  M SLC f t h d ff tt distr-str))
              #f
              '(ok no-icon))))))))

(define compute-warn2 "With a large number of disks this may take some time.")
(define compute-warn3 "Click cancel when you get bored of waiting.")

(define (draw-compute-warning str)
  ((draw-string viewport) posn-warn1 str           black)
  ((draw-string viewport) posn-warn2 compute-warn2 red  )
  ((draw-string viewport) posn-warn3 compute-warn3 red  ))

(define (clear-compute-warning str)
  ((clear-string viewport) posn-warn1 str)
  ((clear-string viewport) posn-warn2 compute-warn2)
  ((clear-string viewport) posn-warn3 compute-warn3))
  
;=====================================================================================================
; Parallelization of the computation of distribution of disks by action-compute.
; Implemented with threads.

(define (distribute n m) ; --> list (k ...) such that (+ k ...) = n and the k's differ by 1 at most.
  (cond
    ((<= n m) (make-list n 1))
    (else
      (define-values (p q) (quotient/remainder n m))
      (append (make-list (- m q) p) (make-list q (add1 p))))))

(define (ranges n) ; Converts a distribution to a list of ranges, one range for each thread.
  (define d (distribute n (processor-count)))
  (for/fold ((i 1) (r '()) #:result (reverse r)) ((k (in-list d)))
    (values (+ i k) (cons (list (sub1 i) (+ i k -1)) r))))

(define //limit 10001)

(define-syntax (posi// stx)
  (syntax-case stx ()
    ((_ m h f t posi)
     #'(cond
         ((< h //limit) ; if n<//limit, then parallelization is not worth the effort.
          (for/list ((d (in-range h))) (posi m h d f t)))
         (else
           (define pool (make-parallel-thread-pool))
           (define threads
             (for/list ((r (in-list (ranges h))))
               (thread
                 (λ ()
                   (for/list ((d (in-range (car r) (cadr r))))
                     (posi m h d f t)))
                 #:pool pool
                 #:keep 'results)))
           (set! running-threads threads)
           (apply append (map thread-wait threads)))))
    ((_ h (m f t posi))
     #'(cond
         ((< h //limit)
          (for/list ((d (in-range h))) (posi m d f t)))
         (else
           (define pool (make-parallel-thread-pool))
           (define threads
             (for/list ((r (in-list (ranges h))))
               (thread
                 (λ ()
                   (for/list ((d (in-range (car r) (cadr r))))
                     (posi m d f t)))
                 #:pool pool
                 #:keep 'results)))
           (set! running-threads threads)
           (apply append (map thread-wait threads)))))))

;=====================================================================================================
; Action compute short.

(define (compute-short h m f t)
  (define (exp2 n) (expt 2 n))
  (define (mod2 n) (modulo n 2))
  (define (mod3 n) (modulo n 3))
  (define (pari n) (add1 (mod2 (add1 n))))
  (define (rotd h d f t) (mod3 (* (- t f) (pari (- h d)))))
  (define (rotr h f t) (rotd h 0 t f))
  (define (mcnt m d) (quotient (+ m (exp2 d)) (exp2 (add1 d))))
  (define (thrd m h f t) (mod3 (+ f (* m (rotr h f t)))))
  (define (onto m h f t) (mod3 (- (thrd m h f t) (rotd h (disk m) f t))))
  (define (from m h f t) (mod3 (+ (thrd m h f t) (rotd h (disk m) f t))))
  (define (posi m h d f t) (mod3 (+ f (* (rotd h d f t) (mcnt m d)))))
  (define (disk m) (sub1 (integer-length (bitwise-xor m (sub1 m)))))
  (values
    (disk m)
    (from m h f t)
    (onto m h f t)
    (posi// m h f t posi)
    #;(for/list ((d (in-range h))) (posi m h d f t))))

;=====================================================================================================
; Action compute long.

(define (compute-long h m f t)
  (define (exp3 n) (expt 3 n))
  (define (mod3 n) (modulo n 3))
  (define (mod4 n) (modulo n 4))
  (define (thrd m   f t) (if (odd? m) t f))
  (define (onto m h f t) (posi m (disk m) f t))
  (define (from m h f t) (- 3 (onto m h f t) (thrd m f t)))
  (define (disk m)
    (let*
      ((log3 (inexact->exact (log 3)))
       (first-guess (ceiling (/ (inexact->exact (log m)) log3)))
       (upper-bound (expt 3 first-guess))
       (lowest-power-3 (gcd m upper-bound)))
      (inexact->exact (round (/ (log lowest-power-3) log3)))))
  #;(define (disk m) (if (zero? (mod3 m)) (add1 (disk (quotient m 3))) 0))
  (define (posi m d f t)
    (case (mod4 (mcnt m d))
      ((0) f)
      ((1 3) (- 3 f t))
      ((2) t)))
  (define (mcnt m d)
    (+
      (* 2  (quotient m (exp3 (add1 d))))
      (mod3 (quotient m (exp3       d)))))
  (values
    (disk m)
    (from m h f t)
    (onto m h f t)
    (posi// h (m f t posi))
    #;(for/list ((d (in-range h))) (posi m d f t))))

;=====================================================================================================
; Action compute circular.

(define (compute-circular h M f t)
  (define (long m h f t r)
    (define-values (d F T distr) (compute-long h m f t))
    (values d F T (append distr (list r))))
  (define (mover h f t r) (values h f t (append (make-list h r) (list t))))
  (cond
    ((= h 1)
     (define r (- 3 f t))
     (case M
       ((1) (values 0 f t (list t)))
       ((2) (values 0 t r (list r)))
       ((3) (values 0 r f (list f)))))
    (else
      (define r (- 3 f t))
      (define h-1 (sub1 h))
      (define 3^<h-1> (expt 3 h-1))
      (define 3^h (* 3 3^<h-1>))
      (define 3^<h-1>-1 (sub1 3^<h-1>))
      (define <3^<h-1>-1>/2 (quotient 3^<h-1>-1 2))
      ; Shift M relative to the second move of the largest disk such that m=0 for this move.
      (define m (modulo (+ M <3^<h-1>-1>/2) 3^h))
      (cond
        ((zero? m)                 (mover  h-1 r f t))
        ((< m 3^<h-1>)             (long m h-1 t r f))
        ((= m 3^<h-1>)             (mover  h-1 f t r))
        ((< m (+ 3^<h-1> 3^<h-1>)) (long m h-1 r f t))
        ((= m (+ (* 2 3^<h-1>)))   (mover  h-1 t r f))
        ((< m (+ (* 3 3^<h-1>) 2)) (long m h-1 f t r))))))

;=====================================================================================================
; Tests. If you don't have it yet, install "https://github.com/joskoot/test.git" before testing.
; The tests are commented out at two places marked with the line "; Commented out?". The last test
; (20) requires action of the user. Follow the displayed instructions.

; Commented out?
#;
(begin 
  
  (require test/test)

  (test 1
    ((define a 1)
     (set! a 2))
    '()
    #:error "set!: cannot mutate identifier")

  (test 2
    ((DEFINE a 1)
     (set! a 2)
     a)
    '(2))

  (test 3
    ((define-values () (values)))
    #f)

  (test 4
    ((define-values (a b c) (values 1 2 3))
     (write (list a b c)))
    #f
    #:output "(1 2 3)")

  (test 5
    ((define-values (a b c) (values 1 2 3))
     (set! a 4))
    '()
    #:error "set!: cannot mutate identifier")

  (test 6
    ((define-values-with-list-of-values the-list (a 1) (b 2) (c 3))
     (list the-list a b c))
    '(((1 2 3) 1 2 3)))

  (test 7
    ((define-values-accumulative () 1 add1))
    '()
    #:error
    "define-values-accumulative: use does not match pattern:
    (define-values-accumulative (id ... last-id) first make-next)")

  (test 8 ; Also check that the start-expr is evaluated once only.
    ((define-values-accumulative (a b c) (begin (writeln 'start) 1)
       (λ (x) (printf "next ~s~n" x) (add1 x)))
     (list a b c))
    '((1 2 3))
    #:output "start next 1 next 2")

  (test 9
    ((idle-limit 0))
    '()
    #:error "Parameter idle-limit: Exact positive integer 1<=time<=10080 wanted. Given 0")
  
  (test 10
    ((idle-limit 20000))
    '()
    #:error "Parameter idle-limit: Exact positive integer 1<=time<=10080 wanted. Given 20000")

  (test 11
    ((idle-limit 1)
     (idle-limit))
    '(1))

  (test 12
    ((idle-limit))
    '(10))
  
  (test 13
    ((define h 3)
     (for/list ((m (in-range 1 (expt 2 h))))
       (call-with-values (λ () (compute-short h m 0 1)) (λ x (cons m x)))))
    '(((1 0 0 1 (1 0 0))
       (2 1 0 2 (1 2 0))
       (3 0 1 2 (2 2 0))
       (4 2 0 1 (2 2 1))
       (5 0 2 0 (0 2 1))
       (6 1 2 1 (0 1 1))
       (7 0 0 1 (1 1 1)))))
  
  (test 14
    ((define h 3)
     (for/list ((m (in-range 1 (expt 3 h))))
       (call-with-values (λ () (compute-long h m 0 1)) (λ x (cons m x)))))
    '((( 1 0 0 2 (2 0 0))
       ( 2 0 2 1 (1 0 0))
       ( 3 1 0 2 (1 2 0))
       ( 4 0 1 2 (2 2 0))
       ( 5 0 2 0 (0 2 0))
       ( 6 1 2 1 (0 1 0))
       ( 7 0 0 2 (2 1 0))
       ( 8 0 2 1 (1 1 0))
       ( 9 2 0 2 (1 1 2))
       (10 0 1 2 (2 1 2))
       (11 0 2 0 (0 1 2))
       (12 1 1 2 (0 2 2))
       (13 0 0 2 (2 2 2))
       (14 0 2 1 (1 2 2))
       (15 1 2 0 (1 0 2))
       (16 0 1 2 (2 0 2))
       (17 0 2 0 (0 0 2))
       (18 2 2 1 (0 0 1))
       (19 0 0 2 (2 0 1))
       (20 0 2 1 (1 0 1))
       (21 1 0 2 (1 2 1))
       (22 0 1 2 (2 2 1))
       (23 0 2 0 (0 2 1))
       (24 1 2 1 (0 1 1))
       (25 0 0 2 (2 1 1))
       (26 0 2 1 (1 1 1)))))
  
  (test 15
    ((define h 3)
     (for/list ((m (in-range 1 (add1 (expt 3 h)))))
       (call-with-values (λ () (compute-circular 3 m 0 1)) (λ x (cons m x)))))
    '((( 1 0 0 1 (1 0 0))
       ( 2 1 0 2 (1 2 0))
       ( 3 0 1 0 (0 2 0))
       ( 4 0 0 2 (2 2 0))
       ( 5 2 0 1 (2 2 1))
       ( 6 0 0 1 (1 0 1))
       ( 7 0 1 2 (2 0 1))
       ( 8 1 0 1 (2 1 1))
       ( 9 0 2 1 (1 1 1))
       (10 0 1 0 (0 1 1))
       (11 1 1 2 (0 2 1))
       (12 0 0 1 (1 2 1))
       (13 0 1 2 (2 2 1))
       (14 2 1 2 (0 0 2))
       (15 0 0 2 (2 0 2))
       (16 0 2 1 (1 0 2))
       (17 1 0 2 (1 2 2))
       (18 0 1 2 (2 2 2))
       (19 0 2 0 (0 2 2))
       (20 1 2 1 (0 1 2))
       (21 0 0 2 (2 1 2))
       (22 0 2 1 (1 1 2))
       (23 2 2 0 (1 1 0))
       (24 0 1 0 (0 1 0))
       (25 0 0 2 (2 1 0))
       (26 1 1 0 (2 0 0))
       (27 0 2 0 (0 0 0)))))

  (test 16
    ((for/list ((n (in-range 100 111))) (distribute n 10)))
    '(((10 10 10 10 10 10 10 10 10 10)
       (10 10 10 10 10 10 10 10 10 11)
       (10 10 10 10 10 10 10 10 11 11)
       (10 10 10 10 10 10 10 11 11 11)
       (10 10 10 10 10 10 11 11 11 11)
       (10 10 10 10 10 11 11 11 11 11)
       (10 10 10 10 11 11 11 11 11 11)
       (10 10 10 11 11 11 11 11 11 11)
       (10 10 11 11 11 11 11 11 11 11)
       (10 11 11 11 11 11 11 11 11 11)
       (11 11 11 11 11 11 11 11 11 11))))

  (test 17
    ((define 1 2))
    '()
    #:error "define: not an identifier")

  (test 18
    ((define-values (a a) #f))
    '()
    #:error "define-values: duplicate identifier")
  
  ; Commented out?
  
  (begin
    (define out-port (current-output-port))
    (displayln "Test 19 lasts a minute. Do not interfere.")
    (displayln "Test 20 passes only when you use the quit button to quit from the GUI.\n")

    (test 19 ; This test takes a minute. Do not click in the GUI. If you want you can close the GUI.
      ((displayln "Test 19 is running and takes a minute. Do not click in the GUI." out-port)
       (displayln "If you want you can close the GUI in the title bar.\n" out-port)
       (idle-limit 1)
       (tower-of-hanoi))
      '()
      #:exn #f
      #:error "Tower of Hanoi No activity during 1 minute. Game aborted.
   Use parameter idle-limit to increase the allowed idle time or use the Idle limit button.")
  
    (test 20 ; To pass this test button quit must be used close the GUI, possibly after other actions.
      ((displayln "Test 20 is running. To pass this test button quit must be used" out-port)
       (displayln "to close the GUI, possibly after other actions.\n" out-port)
       (tower-of-hanoi))
      '()))

  (test-report))

;====================================================================================================
; The end