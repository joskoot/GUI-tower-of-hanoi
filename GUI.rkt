
;=====================================================================================================
; The Tower of Hanoi
; By Jacob J. A. Koot
;=====================================================================================================
;
; A GUI to play the game of The Tower of Hanoi. Moves can be made manually but also automatically by
; the GUI. It has buttons. A click on a button initiates an action. Modal dialogs are used to exchange
; information between the GUI and the user. Module "GUI.scrbl" produces user documentation.
;
;=====================================================================================================

#lang racket/base

;=====================================================================================================

(provide tower-of-hanoi idle-limit)

;=====================================================================================================

(require
  (only-in racket/base
    (define                 DEFINE               )
    (define-values          DEFINE-VALUES        ))
  (only-in racket
    (make-list              make-list            )
    (infinite?              infinite?            )
    (range                  range                )
    (processor-count        processor-count      )
    (~r                     ~r                   ))
  (only-in graphics/graphics
    (open-graphics          open-graphics        )
    (close-graphics         close-graphics       )
    (open-viewport          open-viewport        )
    (open-pixmap            open-pixmap          )
    (close-viewport         close-viewport       )
    (viewport-flush-input   viewport-flush-input )
    (draw-rectangle         draw-rectangle       )
    (draw-solid-rectangle   draw-solid-rectangle )
    (clear-solid-rectangle  clear-solid-rectangle)
    (draw-string            draw-string          )
    (clear-string           clear-string         )
    (get-string-size        get-string-size      )
    (get-mouse-click        get-mouse-click      )
    (ready-mouse-click      ready-mouse-click    )
    (mouse-click-posn       mouse-click-posn     )
    (make-posn              make-posn            )
    (posn-x                 posn-x               )
    (posn-y                 posn-y               )
    (make-rgb               make-rgb             ))
  (only-in racket/gui/base
    (message-box            message-box          )
    (message-box/custom     message-box/custom   )
    (get-text-from-user     get-text-from-user   )
    (get-choices-from-user  get-choices-from-user)
    (message+check-box      message+check-box    )
    (make-eventspace        make-eventspace      )
    (current-eventspace     current-eventspace   ))
  (for-syntax
    (only-in racket/base
      (#%app                %app                 )
      (syntax               syntax               )
      (syntax-case          syntax-case          )
      (with-syntax          with-syntax          )
      (generate-temporaries generate-temporaries )
      (identifier?          identifier?          ))
    (only-in syntax/transformer
      (make-variable-like-transformer
        make-variable-like-transformer           ))))

;=====================================================================================================
; Syntaxes define and define-values are redefined such as to produce immutable variables only. For
; mutable variables DEFINE and DEFINE-VALUES must be used. The value of an immutable variable can be
; mutable, for example a mutable vector or a struct with mutable fields while the variable containing
; the vector or structure is immutable.

(define-syntax (define stx)
  (syntax-case stx ()
    ((_ id value)
     (%app identifier? #'id)
     #'(begin
         (DEFINE var value)
         (define-syntax id (%app make-variable-like-transformer #'var))))
    ((_ (id arg ...       ) body ...)
     #'(define id (procedure-rename (λ (arg ...           ) body ...) 'id)))
    ((_ (id arg . rest-arg) body ...)
     (%app identifier? #'rest-arg)
     #'(define id (procedure-rename (λ (arg     . rest-arg) body ...) 'id)))
    ((_ (id arg ... . rest-arg) body ...)
     #'(define id (procedure-rename (λ (arg ... . rest-arg) body ...) 'id)))))

(define-syntax (define-values stx)
  (syntax-case stx ()
    ((_ (id ...) expr)
     (with-syntax (((var ...) (%app generate-temporaries #'(id ...))))
       #'(begin
           (DEFINE-VALUES (var ...) expr)
           (define id (if (procedure? var) (procedure-rename var 'id) var)) ...)))))

;=====================================================================================================

(define-syntax-rule
  (in-reversed-range n)
  (in-range (sub1 n) -1 -1))

; Define values with in addition a list of these values.

(define-syntax-rule
  (define-with-list the-list (var value) ...)
  (begin
    (define var value) ...
    (define the-list (list var ...))))

; Defines values accumulatively, each one, the first one excepted, made from the previous one by an
; accumulation procedure.

(define-syntax-rule 
  (define-values-accumulative (var ...) first accumulation-procedure)
  (define-values (var ...)
    (apply values
      (for/fold ((val first) (vals '()) #:result (reverse vals))
        ((index (in-range (length '(var ...)))))
        (values (accumulation-procedure val) (cons val vals))))))

;=====================================================================================================
; When the GUI is waiting for a mouse-click or a response to a modal dialog but the user does not act
; or answer within a certain time, the GUI aborts. The limit is hold in parameter idle-limit.

(define default-idle-minutes 10)
(define max-idle-minutes 100000) ; Almost 70 days.
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
    ((_ #f expr) #'(time-out-proc #f (λ () expr)))
    ((_ #t expr) #'(time-out-proc #t (λ () expr)))
    ((_    expr) #'(time-out-proc #f (λ () expr)))))

(define (time-out-proc warn? thunk)
  (when warn?
    ((draw-string viewport) pos-warn1 str-warn1 red)
    ((draw-string viewport) pos-warn2 str-warn2 red))
  (define result-box (box #f))
  (parameterize ((current-eventspace (make-eventspace)))
    (define thrd
      (thread
        (λ () (set-box! result-box (call-with-values thunk list)))
        #:pool 'own
        #:keep 'results))
    (sync/timeout (* (idle-limit) 60) thrd) ; Minutes to seconds.
    (define result (unbox result-box))
    (kill-thread thrd)
    (cond
      (result                               ; Answer received within maximum idle time.
        (when warn?                         
          ((clear-string viewport) pos-warn1 str-warn1)
          ((clear-string viewport) pos-warn2 str-warn2))
        (apply values result))              ; Return the result, possibly a multiple value.
      (else (time-out-abort)))))            ; Maximum idle time exceeded. Abort.

(define (time-out-abort)
  (define limit (idle-limit))
  (fprintf (current-error-port)
    "\nTower of Hanoi\n~
     No activity during ~a. Game aborted.\n~
     Use parameter idle-limit to increase the allowed\n~
     idle time or use the Idle limit button.\n\n"
    (if (= limit 1) "1 minute" (format "~s minutes" limit)))
  (escape))

;===============================================================00000=================================
; Main procedure.
  
(define (tower-of-hanoi)
  (dynamic-wind
    void
    GUI
    close))

(define (GUI)
  (let/ec ec
    (initialize ec)
    (main)))

(define (main) (dispatch (mouse-click-posn (time-out (get-mouse-click viewport)))))

(define (close)
  (close-viewport viewport)
  (close-graphics))

;=====================================================================================================
; Internal state. The following variables can be mutated while playing. They are initialized by
; procedure initialize. After being assigned their value variables viewport and escape never are
; mutated. The viewport can not yet be assigned because this needs graphics to be open. Graphics is
; opened by procedure initialize which also will open and assign the viewport.

(DEFINE height       'mutable)
(DEFINE delay        'mutable)
(DEFINE msg-str      'mutable)
(DEFINE clock        'mutable)
(DEFINE move-count   'mutable)
(DEFINE manual-count 'mutable)
(DEFINE allow-intro  'mutable)
(DEFINE disk-distr   'mutable)
(DEFINE last-compute ""      ) ; Not reinitialized. Memorized between succesive calls to the GUI.
(DEFINE escape       'delayed)
(DEFINE viewport     'delayed)

;=====================================================================================================
; Initialize or reinitialize mutable variables and buttons with content. Open graphics and the
; viewport. Draw the GUI.

(define (initialize ec)
  ; Initialize or reinitialize mutable variables.
  (set! escape         ec)
  (set! height max-height)
  (set! delay       click)
  (set! msg-str        "")
  (set! clock           0)
  (set! move-count      0)
  (set! manual-count    0)
  (set! allow-intro    #t)
  ; Open graphics and the viewport.
  (open-graphics)
  (set! viewport (open-viewport "Tower of Hanoi" vp-width vp-height))
  ; Initialize or reinitialize the buttons with content.
  (button-height 'put-content  height     )
  (button-mode   'put-content 'manual     )
  (button-delay  'put-content  delay      )
  (button-idle   'put-content (idle-limit))
  ; Draw the buttons.
  (for ((button (in-list all-buttons)))
    (draw-button button)
    (when (button2? button) (draw-button-content button (button2-content button))))
  ; Disable button cancel.
  (button-cancel 'disable)
  ; Draw the girder on which the pegs will be mounted.
  ((draw-solid-rectangle viewport) pos-girder (- vp-width (* 2 border)) block gray)
  (for ((p (in-range 0 3)))
    (define str (format "Peg ~s" p))
    (define size (car ((get-string-size viewport) str)))
    ((draw-string viewport)
     (posn-add (make-posn (peg-x p) (- vp-height border 3))
       (- (/ size 2))
       (- (/ str-offset 2))) str white))
  ; Procedure action-reset draws the pegs and places all disks at the left peg.
  ; Also initializes or reinitializes variable disk-distr.
  (action-reset))

;=====================================================================================================
; Variables that can and some of which must be defined in early stage because they will be referenced
; and a variable cannot be referenced before it is defined.

(define block             20                                        )
(define border             (* 3 block)                               )
(define max-height        10                                        )
(define disk-height       block                                     )
(define max-tower-height  (* max-height disk-height)                )
(define str-offset        4                                         )
(define 2*str-offset      (* 2 str-offset)                          )
(define min-disk-width    (* 3 block)                               )
(define disk-width-incr   block                                     )
(define (disk-width d)    (+ min-disk-width (* 2 d disk-width-incr)))
(define max-disk-width    (disk-width (sub1 max-height))            )
(define peg-top           (* 2 block)                               )
(define peg-width         4                                         )
(define click             'click                                    )
(define str-click         (symbol->string click)                    )
(define str-warn1         " A dialog is waiting."                   )
(define str-warn2         " Look for it when you don't see it."     )
(define manual            'manual                                   )
(define white             (make-rgb 1.0 1.0 1.0)                    )
(define black             (make-rgb 0.0 0.0 0.0)                    )
(define gray              (make-rgb 0.6 0.6 0.6)                    )
(define red               (make-rgb 1.0 0.0 0.0)                    )
(define green             (make-rgb 0.0 0.8 0.0)                    )
(define blue              (make-rgb 0.0 0.0 1.0)                    )

(define-with-list strings
  (str-height       " Height "    )
  (str-mode         " Mode "      )
  (str-delay        " Delay "     )
  (str-idle         " Idle limit ")
  (str-reset        " Reset "     )
  (str-setup        " Setup "     )
  (str-quit         " Quit "      )
  (str-cancel       " Cancel "    )
  (str-undo         " Undo "      )
  (str-manual       " manual "    )
  (str-short        " short "     )
  (str-long         " long "      )
  (str-circular     " circular "  )
  (str-compute      " Compute "   ))

;=====================================================================================================
; Dispatch of mouse-clicks.

(define-syntax (dispatch-button stx)
  (syntax-case stx (else)
    ((_ pos (button action ...) ... (else else-action ...))
     #'(let ((p pos))
         (cond
           ((button 'in-button? p) action ...)
           ...
           (else else-action ...))))
    ((_ pos (button action ...) ...)
     #'(let ((p pos))
         (cond
           ((button 'in-button? p) action ...)
           ...)))))

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
      (when   p (action-manual p))))
  ; Always keep running the GUI after the action has been done.
  ; Notice that we do not get here if action-quit has aborted the GUI.
  (main))

(define (dispatch-peg pos)
  (cond
    ((in-region? pos region-peg0) 0)
    ((in-region? pos region-peg1) 1)
    ((in-region? pos region-peg2) 2)
    (else #f)))

;=====================================================================================================
; Buttons. They have procedure property. A button contains its name, its position, its region and a
; boolean indicating whether or not it is enabled. Some buttons contain a content too. When called
; with action in-button? procedures button1 and button2 receive a posn for argument pos cq arg.

(define (proc-button1 button action (pos #f)) ; For the procedure property of buttons without content.
  (case action
    ((in-button?)
     (in-region? pos (button1-region button)))
    ((enabled?)
     (button1-enabled? button))
    ((disable)
     (set-button1-enabled?! button #f)
     (draw-disabled-button  button))
    ((enable)
     (set-button1-enabled?! button #t)
     (draw-button button))))

(define (proc-button2 button action (arg #f)) ; For the procedure property of buttons with content.
  (case action
    ((get-content)
     (button2-content button))
    ((put-content)
     (set-button2-content! button arg)
     (draw-button-content  button arg))
    (else (proc-button1 button action arg))))

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
  (let*
    ((pos position)
     (region (make-region pos with-of-button height-of-button))
     (str-name (str-title-case (symbol->string name))))
    (cond
      (content (make-button2 #t region pos str-name content)) ; Initially #t for field enabled.
      (else    (make-button1 #t region pos str-name)))))      ; Initially #t for field enabled.
             
(define (str-title-case str)
  (define lst (string->list str))
  (list->string (cons (char-upcase (car lst)) (cdr lst))))

; Buttons can be disabled and enabled.

(define (enable/disable-buttons buttons enable/disable)
  (for ((button (in-list buttons))) (button enable/disable)))

;=====================================================================================================
; Procedures drawing buttons and their contents.

(define (draw-button button)
  (define pos (button1-pos button))
  (define name (button1-name button))
  (define x (posn-x pos))
  (define y (posn-y pos))
  ((draw-solid-rectangle viewport) pos with-of-button height-of-button blue)
  ((draw-string viewport)
   (make-posn (+ x str-offset) (+ y height-of-button (- 2*str-offset))) name white))

(define (draw-disabled-button button)
  (define pos (button1-pos button))
  (define name (button1-name button))
  (define x (posn-x pos))
  (define y (posn-y pos))
  ((clear-solid-rectangle viewport) pos with-of-button height-of-button)
  ((draw-rectangle viewport) pos with-of-button height-of-button blue)
  ((draw-string viewport)
   (make-posn (+ x str-offset) (+ y height-of-button (- 2*str-offset))) name blue))

(define (draw-button-content button value)
  (define str (if (string? value) value (format "~a" value)))
  (define pos (button1-pos button))
  (define x (posn-x pos))
  (define y (+ (posn-y pos) height-of-button))
  ((clear-solid-rectangle viewport) (make-posn x y) with-of-button height-of-button)
  ((draw-rectangle viewport)        (make-posn x y) with-of-button height-of-button blue)
  ((draw-string viewport)
   (make-posn (+ x str-offset) (+ y height-of-button (- 2*str-offset))) str blue))

;=====================================================================================================
; Procedures drawing disks and pegs.

(define (draw-pegs)
  (for ((p (in-range 3)))
    ((draw-solid-rectangle viewport)
     (make-posn (peg-x p) peg-y)
     peg-width peg-height green)))

(define (remove-all-disks)
  ((clear-solid-rectangle viewport)
   (make-posn (+ block border) (- vp-height border block max-tower-height))
   (+ (* 3 max-disk-width) (* 2 border))
   max-tower-height)
  (draw-pegs))

(define (draw-disk d h p (color black))
  (define width (disk-width d))
  (define center (+ (peg-x p) (/ peg-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((draw-solid-rectangle viewport) pos width disk-height color)
  ((draw-rectangle viewport) pos width disk-height white)
  ((draw-string viewport)
   (make-posn (- (peg-x p) 2) (+ y block -3))
   (format "~s" d) white))

(define (remove-disk d h p)
  (define width (disk-width d))
  (define center (+ (peg-x p) (/ peg-width 2)))
  (define x (- center (/ width 2)))
  (define y (- vp-height border block (* (add1 h) disk-height)))
  (define pos (make-posn x y))
  ((clear-solid-rectangle viewport) pos width disk-height)
  ((draw-solid-rectangle viewport)
   (make-posn (- center (/ peg-width 2)) y) peg-width disk-height green))

(define (mark-disk d h p) (draw-disk d h p red))

;=====================================================================================================
; Computation of the dimensions of buttons. These depend on the sizes of the texts to be put into the
; buttons. The sizes are determined by procedure get-string-size which requires a viewport or pixmap.
; A temporary pixmap is used. This requires graphics, which is temporarily opened too. Both will be
; closed after finishing the computation.

(define-values (with-of-button height-of-button)
  (let ()
    (open-graphics)
    (define pixmap (open-pixmap "string-sizes" 1000 500))
    (dynamic-wind
      void
      (λ ()
        (define sizes (map (get-string-size pixmap) strings))
        (values
          (+ 2*str-offset (ceiling (inexact->exact (apply max (map car  sizes)))))
          (+ 2*str-offset (ceiling (inexact->exact (apply max (map cadr sizes)))))))
      (λ () (close-viewport pixmap) (close-graphics)))))

;=====================================================================================================
; Lay out of the GUI. Locations and dimensions of all objects to be drawn in the GUI.

(define (posn-add pos width height) (make-posn (+ (posn-x pos) width) (+ (posn-y pos) height)))
(define button-w+b  (+ with-of-button block))
(define peg-y  (* 2 (+ border height-of-button)))
(define peg-height  (+ peg-top max-tower-height))
(define vp-width    (+ (* 3 max-disk-width)   (* 2 block ) (* 4 border)))
(define vp-height   (+ (* 2 height-of-button) (* 3 border) peg-height block))

(define (peg-x p)
  (+ border
    block
    (* p (+ border max-disk-width))
    (/ (- max-disk-width peg-width) 2)))

(define-values-accumulative
  (pos-height
    pos-mode
    pos-delay
    pos-idle
    pos-reset
    pos-setup
    pos-quit
    pos-cancel
    pos-peg0
    pos-peg1
    pos-peg2
    pos-compute)
  (make-posn border border)
  (λ (pos) (posn-add pos button-w+b 0)))

(define pos-msg    (posn-add  pos-idle    button-w+b (- (* 2 height-of-button) str-offset       )))
(define pos-warn1  (posn-add  pos-compute button-w+b (-   (+ height-of-button  str-offset) block)))
(define pos-warn2  (posn-add  pos-warn1   0          block                                       ))
(define pos-girder (make-posn border      (- vp-height border block))                             )

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
           (+ border block)
           (- vp-height border block peg-height)))
       (regions '()) #:result regions)
      ((n (in-range 3)))
      (values
        (posn-add pos (+ max-disk-width border) 0)
        (cons (make-region pos max-disk-width peg-height) regions)))))

;=====================================================================================================
; Now define the buttons. Contents are mutable, but the initial contents always are the same, with
; exception of that of button-idle, whose initial value is taken from parameter idle-limit.

(define-with-list all-buttons
  (button-height  (make-button 'height       pos-height max-height  ))
  (button-mode    (make-button 'mode         pos-mode   manual      ))
  (button-delay   (make-button 'delay        pos-delay  click       ))
  (button-idle    (make-button '|idle limit| pos-idle   (idle-limit)))
  (button-reset   (make-button 'reset        pos-reset              ))
  (button-setup   (make-button 'setup        pos-setup              ))
  (button-quit    (make-button 'quit         pos-quit               ))
  (button-cancel  (make-button 'cancel       pos-cancel             ))
  (button-peg0    (make-button '|peg 0|      pos-peg0               ))
  (button-peg1    (make-button '|peg 1|      pos-peg1               ))
  (button-peg2    (make-button '|peg 2|      pos-peg2               ))
  (button-compute (make-button 'compute      pos-compute            )))

;=====================================================================================================
; When validating a modal dialog that returns a string, data must be read from the string, possibly
; followed by some computation. We don't want the GUI to crash when the user provides wrong data. The
; validator simply must reject the answer given by the user. Therefore the following handler. 

(define-syntax-rule
  (catch-exn expr ...)
  (with-handlers ((exn:fail? catch-exn:fail)) expr ...))

(define (catch-exn:fail e) #f)

;=====================================================================================================
; Action manual.

(define (action-manual from-peg)
  (define peg-disks (vector-ref disk-distr from-peg))
  (unless (null? peg-disks)
    (define d (car peg-disks))
    (when ; Act only if the selected disk can be moved, else do nothing.
      (or
        (< d (size-of-top-disk (modulo (+ 1 from-peg) 3)))
        (< d (size-of-top-disk (modulo (+ 2 from-peg) 3))))
      (define h (sub1 (length peg-disks)))
      (mark-disk d h from-peg)
      (action-manual1 d h from-peg))))

(define (action-manual1 d h from-peg)             ; Let's see at which peg to put the disk.
  (button-cancel 'enable)
  (define pos (mouse-click-posn (time-out (get-mouse-click viewport))))
  (dispatch-button pos
    (button-peg0 (action-manual2 d h from-peg 0)) ; Yes, peg 0 selected.
    (button-peg1 (action-manual2 d h from-peg 1)) ; Yes, peg 1 selected.
    (button-peg2 (action-manual2 d h from-peg 2)) ; Yes, peg 2 selected.
    (button-cancel                                ; Move canceled. Unmark the selected disk.
      (draw-disk d h from-peg)
      (button-cancel 'disable)
      (reset-manual-count))
    (else                                         ; Disk not selected by means of a peg button.
      (define dest-peg (dispatch-peg pos))        ; May be by a click near a peg.
      (cond                                       ; Use manual2 to move the disk to peg dest.
        (dest-peg (action-manual2 d h from-peg dest-peg))
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

(define (size-of-top-disk p)
  (define peg (vector-ref disk-distr p))
  (if (null? peg)
    max-height                           ; Size of a virtual top disk at an empty peg.
    (car peg)))

;=====================================================================================================
; Show the number of manual moves.

(define (increment-manual-count)
  (set! manual-count (add1 manual-count))
  (clear-manual-count)
  (draw-manual-count))

(define (clear-manual-count) ((clear-string viewport) pos-msg msg-str))

(define (draw-manual-count)
  (when (> manual-count 0)
    (clear-manual-count)
    (set! msg-str (format "Manual moves ~s" manual-count))
    ((draw-string viewport) pos-msg msg-str)))

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
; Action mode.
; When finishing mode short, long or circular, notify the user.
; Disable/enable all buttons, reset, quit and cancel excepted.
; The latter allow aborting the short, long and circular action.

(define (finish who)
  (time-out #t (message-box who "\n\n\nfinished\n\n\n" #f '(ok no-icon)))
  (viewport-flush-input viewport) ; Ignore mouse-clicks made before a response on the dialog.
  (prepare/finish-action-mode 'enable)
  (button-cancel 'disable)
  ((clear-string viewport) pos-msg msg-str)
  (button-mode 'put-content 'manual))

(define buttons-for-action-mode   ; All buttons, reset, quit and cancel excepted.
  (remove* (list button-reset button-quit button-cancel) all-buttons))

(define (prepare/finish-action-mode enable/disable)
  (enable/disable-buttons buttons-for-action-mode enable/disable))

(define modes (list str-short str-long str-circular))

(define (action-mode)
  (prepare/finish-action-mode 'disable)
  (define choice
    (time-out #t
      (get-choices-from-user
        str-mode
        "Select a mode\nCancel in order to remain in manual mode."
        modes
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
  (let/ec ec
    ; The exit allows procedure move-disk to stop the action.
    (define (exit) (clear-msg) (ec))
    (define p-list
      (for*/list
        ((d (in-reversed-range height))
         (p (in-range 3))
         #:when (member d (vector-ref disk-distr p)))
        p))
    (define (short conf dest)
      (cond
        ((null? conf))
        ((= (car conf) dest) (short (cdr conf) dest))
        (else
          (define new-conf (- 3 (car conf) dest))
          (short (cdr conf) new-conf)
          (move-disk (car conf) dest exit)
          (short (make-list (length (cdr conf)) new-conf) dest))))
    (short p-list 2)))

;=====================================================================================================
; Action long mode.

(define (long)
  (action-reset)
  (reset-time-and-move-counter)
  (let/ec ec
    ; The exit allows procedure move-disk to stop the action.
    (define (exit) (clear-msg) (ec))
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
  (let/ec ec
    ; The exit allows procedure move-disk to stop the action.
    (define (exit) (clear-msg) (ec))
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
  (set! msg-str "")
  (draw-count-msg))

(define (clear-msg) ((clear-string viewport) pos-msg msg-str))

(define (draw-count-msg)
  (clear-msg)
  (set! move-count (add1 move-count))
  (set! msg-str
    (if (eq? delay click)
      (format "Move count: ~s" move-count)
      (format "Move count: ~s, real time: ~a seconds" move-count (watch-clock))))
  ((draw-string viewport) pos-msg msg-str))

(define (watch-clock)
  (~r #:precision (list '= 3) (/ (- (current-inexact-milliseconds) clock) 1000)))

;=====================================================================================================
; Actions short, long and circular use procedure move-disk.
; It allows abort from the action by means of buttons reset and cancel.
; For this purpose procedure move-disk receives a continuation of the calling action.
; If the delay is not click it uses procedure doze in order to make the moves at the desired rate.

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

(define (doze t exit) ; t in seconds. Like sleep, but with time out.
  (cond
    ((zero? delay) (doze-help exit))
    (else
      (define starting-time (current-inexact-milliseconds))
      (define finish-time (+ starting-time (* 1000 t)))
      (define sleeping-time (min 0.25 (/ delay 1.01))) ; Periodically sleep somewhat shorter
      (define (doze-loop)                              ; and check for reset, cancel and quit.
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
    (and (<= 1 (string-length str) 6)
      (catch-exn
        (define input (open-input-string str))
        (define idle-limit (read input))
        (and (exact-positive-integer? idle-limit)
          (<= min-idle-minutes idle-limit max-idle-minutes)))))
  (define str
    (time-out #t
      (get-text-from-user
        str-idle
        (format
          "Enter an exact positive integer number not exceeding ~s\n~
           for the maximally allowed idle time in minutes.\n~
           Do not enter more than 6 characters."
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
  (remove-all-disks)
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
  (set! msg-str "Setting up")
  (remove-all-disks)
  (set! disk-distr (make-vector 3 '()))
  ((draw-string viewport) pos-msg msg-str red)
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

(define-syntax (accept-cancel stx)
  (syntax-case stx ()
    ((_ expr) #'(accept-cancel-proc (λ () expr)))))

(DEFINE running-threads #f)

(define (accept-cancel-proc thunk)
  (define result-box (box #f))
  (define task
    (thread (λ () (set-box! result-box (call-with-values thunk list)))))
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

(DEFINE-VALUES (SLC h M m f t) (values #f #f #f #f #f #f)) ; Procedure validate-compute sets these
(define namespace (make-base-namespace))
(define (catch-exn-for-compute e) (set! SLC 'wrong))
(define nr-of-disks-per-line 50)

(define (validate-compute str)
  (with-handlers ((exn:fail? catch-exn-for-compute))
    (define input (open-input-string str))
    (set! SLC (read input))
    (set! h (read input)) (namespace-set-variable-value! 'h h #t namespace #f)
    (set! M (read input))
    (set! m (if (exact-positive-integer? M) M (eval M namespace)))
    (set! f (read input))
    (set! t (read input))
    (unless (eof-object? (read input)) (raise (make-exn:fail 'ignored))))
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
    (set! SLC 'wrong)))

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
                 height: number of disks (can be greater than 9).\n  ~
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
  (enable/disable-buttons buttons-for-action-compute 'enable)
  (button-cancel 'disable))

(define (compute-help first?)
  (define str
    (time-out #t
      (get-text-from-user str-compute
        (string-append
          (if first? "" "Wrong data, try again or cancel.\n")
          "Give mode, height, move, from-disk and onto-disk\n")
        #f
        last-compute)))
  (viewport-flush-input viewport)
  (set! last-compute str)
  (when str
    (validate-compute str)
    (cond
      ((eq? SLC 'wrong) (compute-help #f))
      (else
        (button-cancel 'enable)
        (enable/disable-buttons buttons-for-action-compute 'disable)
        (define result
          (accept-cancel
            ((case SLC
               ((S) compute-short)
               ((L) compute-long)
               ((C) compute-circular))
             h m f t)))
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
    
;=====================================================================================================
; Parallelization of the computation of distribution of disks by action-compute.

(define (distribute n m)
  (cond
    ((<= n m) (make-list n 1))
    (else
      (define-values (p q) (quotient/remainder n m))
      (append (make-list (- m q) p) (make-list q (add1 p))))))

(define (ranges n)
  (define d (distribute n (processor-count)))
  (for/fold ((i 1) (r '()) #:result (reverse r)) ((k (in-list d)))
    (values (+ i k) (cons (list (sub1 i) (+ i k -1)) r))))

(define-syntax (posi// stx)
  (syntax-case stx ()
    ((_ m h f t posi)
     #'(let () 
         (define threads
           (for/list ((r (in-list (ranges h))))
             (thread
               (λ ()
                 (for/list ((d (in-range (car r) (cadr r))))
                   (posi m h d f t)))
               #:pool 'own
               #:keep 'results)))
         (set! running-threads threads)
         (apply append (map thread-wait threads))))
    ((_ h (m f t posi))
     #'(let () 
         (define threads
           (for/list ((r (in-list (ranges h))))
             (thread
               (λ ()
                 (for/list ((d (in-range (car r) (cadr r))))
                   (posi m d f t)))
               #:pool 'own
               #:keep 'results)))
         (set! running-threads threads)
         (apply append (map thread-wait threads))))))

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
      (define m (modulo (+ M <3^<h-1>-1>/2) 3^h))
      (cond
        ((zero? m)                 (mover  h-1 r f t))
        ((< m 3^<h-1>)             (long m h-1 t r f))
        ((= m 3^<h-1>)             (mover  h-1 f t r))
        ((< m (+ 3^<h-1> 3^<h-1>)) (long m h-1 r f t))
        ((= m (+ (* 2 3^<h-1>)))   (mover  h-1 t r f))
        ((< m (+ (* 3 3^<h-1>) 2)) (long m h-1 f t r))))))

;=====================================================================================================
; The end
