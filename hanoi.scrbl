#lang scribble/manual
@;----------------------------------------------------------------------------------------------------
@(require scribble/base scribble/core scribble/eval racket
   #; (only-in "hanoi.rkt" vp-width vp-height)
   (for-label "hanoi.rkt" racket
     (only-in typed/racket Setof Natural Sequenceof Index))
   (for-syntax racket))

@(define-for-syntax local #f)

@(printf "Directory: ~s~n" (path->string (current-directory)))

@(define-syntax-rule
   (Interaction x ...)
   (interaction/no-prompt
     #:eval
     (make-base-eval
       #:pretty-print? #f
       #:lang
       '(begin
          (require racket/base)
          (print-reader-abbreviations #f)
          (define (hex str) (read (open-input-string str)))))
     x ...))

@(define-syntax-rule
   (Interaction* x ...)
   (interaction/no-prompt #:eval evaller x ...))

@(define (make-evaller)
   (make-base-eval
     #:pretty-print? #f
     #:lang
     '(begin
        (require racket)
        (print-reader-abbreviations #f)
        (define (hex str) (read (open-input-string str))))))

@(define-syntax (Defmodule stx)
   (if local
     #'(defmodule "hanoi.rkt" #:packages ())
     #'(defmodule hanoi/hanoi #:packages ())))

@(define (reset-Interaction*) (set! evaller (make-evaller)))
@(define-syntax (ignore stx) #'(void))
@(define evaller (make-evaller))
@(define nb nonbreaking)
@(define ↑ superscript)
@(define ↓ subscript)
@(define lb linebreak)
@(define(minus) (larger (tt "-")))

@title[#:version ""]{Tower of Hanoi}
@author{Jacob J. A. Koot}

@(Defmodule)

@section[#:style '(unnumbered)]{Introduction}

The @hyperlink["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of Hanoi} is a game.
It has 3 pegs, one at the left, one in the middle and one at the right.
It has a number of disks.
The disks have different diameters and a hole in the center.
They are put at the pegs.
Initially all disks are at the peg at the left,
forming a conical tower with the disks in order of decreasing size from bottom to top.
See the @elemref["figure"]{figure} below.
The goal of the game is to move all disks to the peg at the right by making successive moves.
A move is made by taking the top disk of a non-empty peg and putting it on top onto another peg
or just putting it there if the peg of
des@element['roman ?-]ti@element['roman ?-]na@element['roman ?-]tion has no disks.
However, @nb{it is} not allowed to put a disk upon a smaller one.
Hence, @nb{a disk} never rests upon a smaller one and
except while being moved it always is at a peg.

@section[#:style '(unnumbered)]{How to play}

@defproc[(tower-of-hanoi) void?]{
 Opens a GUI for playing the game of the
 @hyperlink["https://en.wikipedia.org/wiki/Tower_of_Hanoi"]{Tower of Hanoi}.
 It looks like this:

 @elemtag["figure"]
 
 @(hspace 3)@image["gui-pict.gif" #:scale 0.45]

 The user can instruct the GUI which action to take by clicking one of the blue buttons
 and by clicking nearby a peg. The actions are described in the sections to follow.
 A button can temporarily be disabled when not applicable during the current action,
 in which case it turns white.
 For some actions the GUI asks a question in a separate modal dialog.
 Instructions given before the question is answered are ignored.
 The width of the GUI is 1540 pixels,
 the height 494 pixels,
 title bar and border not included.}

@defparam[idle-limit time (and/c exact-positive-integer? (<=/c 100000)) #:value 10]{
 When the GUI is waiting for a mouseclick or an answer to a modal dialog
 but receives no response within @racket[time] minutes, the GUI aborts.
 Within the GUI the limit can be adjusted by means of
 the @seclink["Idle limit"]{idle limit} button.
 The maximum limit of 100000 minutes is almost 70 days.}

@subsection[#:tag "Height"]{Height}

Opens a modal dialog for selection of the desired number of disks,
at least one, at most ten.
Initially the height is 10.

@subsection[#:tag "Mode" ""]{Mode}

Opens a modal dialog for selection of the mode, which is manual, short, long or circular.
Initially the mode is manual.
This mode is not included in the dialog.
The GUI starts in mode manual and actions short,
long and circular return to manual mode after completion or @seclink["Cancel"]{cancelation}.

In manual mode the user is supposed to click the @seclink["Peg n"]{peg} button
the disk is to be taken from followed by a click on the @seclink["Peg n"]{peg} button of
des@element['roman ?-]ti@element['roman ?-]na@element['roman ?-]tion.
In stead of clicking a @seclink["Peg n"]{peg} button one can click nearby the corresponding peg.
The disk selected to be moved is colored red.
@nb{The selection} can be canceled by clicking nearby the peg were it is
or clicking the corresponding @seclink["Peg n"]{peg} button
or by clicking the @seclink["Cancel"]{cancel} button.
An attempt to make an illegal move is ignored.

In short mode the disks are moved by the GUI to the peg at the right
with the least possible number of moves,
at most @tt{2@↑{h}-1} moves, where @tt{h} is the @seclink["Height"]{height}.
Exactly @tt{2@↑{h}-1} moves when starting with all disks at @seclink["Peg n"]{peg} 0 or 1.
The shortest way always is uniquely defined.

When the long mode is selected, first all disks are placed at the peg at the left and
subsequently moved to the peg at the right with the largest number of moves possible
without passing any distribution of disks more than once.
@tt{3@↑{h}-1} moves, where @tt{h} is the @seclink["Height"]{height}.
In fact every legal distribution of disks is visited.
@nb{The sequence} of moves is uniquely defined.

When the circular mode is selected, first all disks are placed at the peg at the left and
subsequently moved such as to pass exactly once along every legal distribution of disks and finishing
with all disks at the peg started from.
@tt{3@↑{h}} moves, where @tt{h} is the @seclink["Height"]{height}.
@nb{The sequence} of moves is uniquely defined when we ignore the fact that the moves can be made
in reversed order too.

The short, long and circular mode can be aborted by clicking the
@seclink["Reset"]{reset} or @seclink["Cancel"]{cancel} button,
which make the GUI return to manual mode.

@subsection[#:tag "Delay"]{Delay}

The delay is specified by means of a modal dialog.
It is either @tt{click} or a non-negative real number
written with not more than 6 characters.
It applies to @seclink["Mode"]{modes} short, long and circular.
The initial value is @tt{click}.

When the delay is @tt{click} a move is made after each mouseclick
at arbitrary position in the window of the GUI
but not on the @seclink["Reset"]{reset} or @seclink["Cancel"]{cancel} button,
which terminate the @seclink["Mode"]{modes} short, long and circular
and return to manual @seclink["Mode"]{mode}.

If the delay is a non-negative real number, say d,
the GUI waits d seconds between successive moves.
In fact the delay is d+ε seconds,
ε being the minimum time for a single move,
which is the non-zero real time the GUI needs to make a move.
ε depends on your CPU and GPU and the load by other programs. It may be about 1 ms.
 
@subsection[#:tag "Idle limit"]{Idle limit}

Opens a dialog to adjust the idle limit in minutes.
When the GUI is waiting for a mouse@element['roman ?-]click or an answer to a dialog but
does not receive such click or answer within idle limit minutes, it halts.
The initial idle limit is collected from parameter @racket[idle-limit].
The limit must be an exact positive integer less than or equal to 100000.
This limit is almost 70 days.

@subsection[#:tag "Reset"]{Reset}

Puts all disks at the peg on the left.
May cancel a current action in order to return to @seclink["Mode"]{mode} manual.

@subsection[#:tag "Setup"]{Setup}

Removes all disks and subsequently places disks at the pegs in a
distribution chosen by the user.
Disks are placed in order of decreasing size.
@nb{The user} is supposed to click a @seclink["Peg n"]{peg} button or nearby the corresponding peg
indicating where each next disk is to be placed.
Requires @seclink["Height"]{height} such clicks.
Click the @seclink["Reset"]{reset} or @seclink["Cancel"]{cancel} button to cancel setup.

@subsection[#:tag "Quit"]{Quit}

Closes and terminates the GUI after asking for confirmation in a modal dialog.
Does nothing if not confirmed.

@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))

@note{The window of the GUI can be closed by means of the close button in the title bar
 (at the top-right corner),
 but this does not terminate immediately procedure @racket[tower-of-hanoi]
 because it will keep waiting for a mouseclick.
 However, after closing the GUI window, no such mouseclick can be made.
 Never@element['roman ?-]the@element['roman ?-]less, the GUI eventually will terminate.
 See the @seclink["Idle limit"]{idle limit} button and parameter @racket[idle-limit].
 A modal dialog may keep responding but eventually the GUI will need a mouseclick other than
 an answer to a dialog.}

@subsection[#:tag "Cancel"]{Cancel}

This button is activated for actions that may be canceled.
It returns to manual @seclink["Mode"]{mode} and
may have the side effect of @seclink["Reset"]{resetting} the disks at the pile at the left.

@subsection[#:tag "Peg n"]{Pegs 1, 2 and 3}

Used to make moves manually and for @seclink["Setup"]{setup} of a distribution of disks.
@nb{The same} can be done by clicking nearby the corresponding peg.

@subsection[#:tag "Compute"]{Compute}

Calculates a move and the resulting distribution of disks
for the shortest path, the longest path, or the circular path,
where all three start and end with all disks on one pin.
Opens two dialog boxes.
The first one is for information only and can be suppressed.
The second one wants the following data:

@inset[@tabular[
 (list
   (list @tt{mode}
     @list["capital letter: "@tt{S}" for short, "@tt{L}" for long and "@tt{C}" for circular."])
   (list @tt{h} "number of disks: exact positive integer.")
   (list @tt{m} "move number: expression yielding a positive exact integer.")
   (list @tt{f} @list["starting peg: "@tt{0}", "@tt{1}" or "@tt{2}"."])
   (list @tt{t}
     @list["destination peg: "@tt{0}", "@tt{1}" or "@tt{2}" but not the same as the starting peg."]))
 #:column-properties '(center left)
 #:sep (hspace 2)]]

If @tt{m} not already is a an exact positive integer,
it is assumed to be an expression and is evaluated with procedure @racket[eval] in a
@seclink["Namespaces" #:doc '(lib "scribblings/reference/reference.scrbl")]{base-namespace}
to which a mutable variable h is added with value @tt{h}.
Moves are counted starting from 1.
The move number @tt{m} and height @tt{h} must satisfy the following rules:

@inset{@tabular[
 (list
   (list "Short mode" @tt{1≤m<2@↑{h}})
   (list "Long mode" @tt{1≤m<3@↑{h}})
   (list "Circular mode" @tt{1≤m≤3@↑{h}}))
 #:sep (hspace 1)]}

The data provided by the user are memorised between successive compute actions and between
successive calls to procedure @racket[tower-of-hanoi].
There is no limit to the number of disks, but a very large height, say 1000000 disks,
can take much time, involving exact numeric operations on very large numbers, about
10@↑[@(format "~s" (order-of-magnitude (expt 3 1000000)))].
@nb{For reasonable} heights, say up to 10000 disks,
the computation is fast because it is not recursive in the sense that
it does not depend on preceding moves or solutions with less disks.
For a circular path the des@element['roman ?-]ti@element['roman ?-]na@element['roman ?-]tion
determines the order of visited distributions with all disks at one peg: starting peg,
des@element['roman ?-]ti@element['roman ?-]na@element['roman ?-]tion peg,
the remaining third peg and finally back to the starting peg.

@note{The computation of the resulting distribution of disks can be parallelized.
 Code with @seclink["futures" #:doc '(lib "scribblings/reference/reference.scrbl")]{futures}
 for this pupose is already present in the source code and gives correct answers,
 but does not parallelize yet.}

@section[#:style '(unnumbered)]{Appendix}

@subsection{Graphical representation}

Let @tt{h} be the number of disks.
@nb{The disks} can be distributed among the pegs in 3@↑{@tt{h}} ways.
@nb{The distributions} can be taken as the vertices of a connected graph
with the moves as bidirectional edges of length one.
Connectivity means that there is at least one path between every two vertices.
@nb{The graph} can be drawn such as to resemble a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}.
For example, for 5 disks:

@(hspace 5) @image["hanoi-whole-5.gif" #:scale 0.25]

In contrast to a
@hyperlink["https://en.wikipedia.org/wiki/Sierpi%C5%84ski_triangle"]{Sierpińsky triangle}
@nb{the vertices} of the triangles of a Hanoi graph do not coincide with the vertices
of other triangles.
The vertices of two triangles are separated by at least one edge.
The three vertices of the largest triangle represent distributions with all disks at the same peg.
These have two edges only,
the two moving the smallest disk to one of the two empty pegs.
All other vertices of the graph represent distributions with at most one empty peg.
Such a vertex has three edges. Let Pa, Pb and Pc
be the three pegs in increasing order of the size of the disk on top.
If there is an empty peg, take that one as Pc.
There are three edges: Pa→Pb, Pa→Pc and Pb→Pc.
The graph has
@nb{@tt{(3(3@↑{h}@(minus)3)+3×2)/2)}} =
@nb{@tt{(3@↑{h+1}@(minus)@smaller{3})/2}}
edges because it has @tt{h}@↑{3} vertices, of which
@tt{h@↑{3}@(minus)3} have @nb{3 edges} and 3 vertices have 2 edges only.
The division by 2 is needed because every edge connects 2 vertices.
Because the dividend is even, the division yields an integer number.

Consider paths from a distribution of all disks at the same peg
to a distribution with all disks at another peg.
These distributions are represented by the vertices of the largest triangle.
The least number of moves required is @tt{h}@↑{2}@tt{-}1 with uniquely defined
sequence of moves. This is the short mode, in the graph shown by the sides of the largest triangle.

The largest number of moves without passing a distribution of disks among the pegs more than once
is @tt{h}@↑{3}@(minus)1, implying that every legal distribution is visited exactly once.
This is the long mode and is uniquely defined too.
For three disks with labels showing the positions (pegs)
of the disks from left to right in order of increasing disk size: 

@(hspace 5) @image["long-3.gif" #:scale 0.45]

Another interesting way is the circular mode, moving the disks such as to visit all legal
distributions of disks among the pegs exactly once
and finishing with all disks on the starting peg.
This takes @tt{h}@↑{3} moves. @nb{The circular} mode is uniquely defined too when
disregarding the fact that the moves can be made in reversed order too. For three disks:

@(hspace 5) @image["circular3.gif" #:scale 0.45]

@subsection{Number of paths}

The number of distinct non self-crossing paths from a distribution with all disks at the same peg
to one with all disks at another peg, those not passing all vertices included,
is a(n) with @nb{a(0)=1} and
@nb{a(n+1)=a(n)@↑{2}+a(n)@↑{3}}.
This is a very fast increasing sequence.
See @hyperlink["https://oeis.org/A125295"]{A125295} of @hyperlink["https://oeis.org/"]{OEIS}.

@subsection{Non recursive formulas}

Regarding elementary arithmetic operations as non-recursive,
given height @tt{h}, starting peg @tt{f} and
des@element['roman ?-]ti@element['roman ?-]na@element['roman ?-]tion peg @tt{t},
all information about the @tt{m}@↑{th} move
along the shortest path from peg @tt{f} to peg @tt{t}
and the resulting distribution of disks
can be computed without recursion, in particular without dependence on previous moves
or a solution with less disks.
Written in @hyperlink["https://www.scheme.org/"]{Scheme} or
@hyperlink["https://racket-lang.org/"]{Racket} as follows:

@elemtag["example" ""]

@tabular[
 (list
   (list "Legend" @tt{m}
     (element #f
       (list
         "move number, starting from "
         @tt{1}
         ".")))
   (list "" @tt{h}
     (element #f
       (list
         "height of the tower, id est the number of disks. "
         @tt{h}
         ">"
         @tt{0}
         ".")))
   (list "" @tt{f}
     (element #f
       (list
         "starting peg, "
         @tt{0}
         ", "
         @tt{1}
         " or "
         @tt{2}
         ".")))
   (list "" @tt{t}
     (element #f
       (list
         "destination peg, "
         @tt{0}
         ", "
         @tt{1}
         " or "
         @tt{2}
         ", but "
         @tt{t}
         "≠"
         @tt{f} ".")))
   (list "" @tt{d}
     (element #f
       (list
         "disk, "
         @tt{0} "≤"
         @tt{d}"<"
         @tt{h}
         ", in order of size, "
         @tt{0}
         " being the smallest disk."))))
 #:sep (hspace 2)]

@Interaction*[
 (define (exp2 n        ) (expt   2 n))
 (define (mod2 n        ) (modulo n 2))
 (define (mod3 n        ) (modulo n 3))
 (define (pari n        ) (add1 (mod2 (add1 n))))
 (define (rotd   h d f t) (mod3 (* (- t f) (pari (- h d)))))
 (define (rotr   h   f t) (rotd h 0 t f))
 (define (mcnt m   d    ) (quotient (+ m (exp2 d)) (exp2 (add1 d))))
 (define (thrd m h   f t) (mod3 (+ f (* m (rotr h f t)))))
 (define (onto m h   f t) (mod3 (- (thrd m h f t) (rotd h (disk m) f t))))
 (define (from m h   f t) (mod3 (+ (thrd m h f t) (rotd h (disk m) f t))))
 (define (posi m h d f t) (mod3 (+ f (* (rotd h d f t) (mcnt m d)))))
 (define (disk m        ) (sub1 (integer-length (bitwise-xor m (sub1 m)))))]

@tt{(disk m)} identifies the disk being moved during move @tt{m}
and is the number of times @tt{m} can be divided by @tt{2}.
@tt{from}, @tt{onto} and @tt{thrd} are the
peg the disk is taken from, the peg it is moved to and the remaining third peg.
@tt{posi} computes the position of disk @tt{d} after move @tt{m}.
                                        
@Interaction*[
 (time
   (begin
     (define h 80)
     (define N-Avogadro #e6.02214076e23)
     (define f 0)
     (define t 2)
     (printf "Length of the whole path: ~s~n" (sub1 (expt 2 h)))
     (printf "Disk     : ~s~n" (disk N-Avogadro))
     (printf "From peg : ~s~n" (from N-Avogadro h f t))
     (printf "Onto peg : ~s~n" (onto N-Avogadro h f t))
     (printf "Thrd peg : ~s~n" (thrd N-Avogadro h f t))
     (printf "Positions of the disks in the resulting distribution~n")
     (printf "of disks in increasing order their sizes~n")
     (for ((d (in-range 80)))
       (when (= d 40) (newline))
       (printf "~s" (posi N-Avogadro h d f t)))
     (newline)
     (printf "~nTimes in ms: ")))]

@(reset-Interaction*)

Similar formulas exist for the longest non-selfcrossing path from @tt{f} to @tt{t}:

@Interaction*[
 (define (exp3 n) (expt   3 n))
 (define (mod3 n) (modulo n 3))
 (define (mod4 n) (modulo n 4))
 (define (thrd m   f t) (if (odd? m) t f))
 (define (onto m h f t) (posi m (disk m) f t))
 (define (from m h f t) (- 3 (onto m h f t) (thrd m f t)))
 (define (disk m) (if (zero? (mod3 m)) (add1 (disk (quotient m 3))) 0))
 (code:comment " ")
 (code:comment "(disk m) is recursive.")
 (code:comment #,(list "It is possible, to make it non-recursive. "
                   @elemref["nonrrec-disk"]{See below}))
 (code:comment " ")
 (define (posi m d f t)
   (case (mod4 (mcnt m d))
     ((0) f)
     ((1 3) (- 3 f t))
     ((2) t)))
 (code:comment " ")
 (define (mcnt m d)
   (+
     (* 2  (quotient m (exp3 (add1 d))))
     (mod3 (quotient m (exp3 d)))))]

@elemtag["nonrrec-disk" ""]
Below a non-recursive version of procedure disk when regarding exponentiation and taking a logarithm
as non-recursive.

@note{Works for m up to 3@↑{5×10@↑{7}}, probably even much more,
 but is not guaranteed to work for every m because of the use of logarithms and inexact numbers.}

@racketblock[
 (define (disk m)
   (let*
     ((log3 (inexact->exact (log 3)))
      (first-guess (ceiling (/ (inexact->exact (log m)) log3)))
      (upper-bound (expt 3 first-guess))
      (lowest-power-3 (gcd m upper-bound)))
     (round (inexact->exact (/ (log lowest-power-3) log3)))))]

The following example resembles the one for the shortest path,
but the differences are such that making a procedure that can handle both examples
would make the code less easy to read.

@Interaction*[
 (time
   (begin
     (define h 80)
     (define N-Avogadro #e6.02214076e23)
     (define f 0)
     (define t 2)
     (printf "Length of the whole path: ~s~n" (sub1 (expt 3 h)))
     (printf "Disk     : ~s~n" (disk N-Avogadro))
     (printf "From peg : ~s~n" (from N-Avogadro h f t))
     (printf "Onto peg : ~s~n" (onto N-Avogadro h f t))
     (printf "Thrd peg : ~s~n" (thrd N-Avogadro   f t))
     (printf "Positions of the disks in the resulting distribution~n")
     (printf "of disks in increasing order their sizes~n")
     (for ((d (in-range 80)))
       (when (= d 40) (newline))
       (printf "~s" (posi N-Avogadro d f t)))
     (printf "~nTimes in ms: ")))]

The formulas are used for button @seclink["Compute"]{compute} only.
When walking a whole path, recursion is faster because it can use information about
what happened during previous moves,
but when wanting the information for one move only, for example
move 6.02214076×10@↑{23}
@hyperlink["https://en.wikipedia.org/wiki/Avogadro_constant"]{(Avogadro's number)}
for a tower of 80 disks,
the formulas produce results instantaneously without the need to pass along previous moves.
See the @elemref["example"]{examples above}.

@subsection{Counting distributions}

Let h be the number of disks, h≥3.
A distribution of these h disks can have one or two empty pegs.
Obviously there are three distributions with two empty pegs.
There are three ways to select two pegs out of three
and 2@↑{h} ways to distribute the disks among these two pegs.
However, two of these distributions leave the other peg empty.
Hence given a selection of the two pegs we find @nb{2@↑{h}@(minus)2} distributions and
because the two pegs can be chosen in three ways, we find a total of @nb{3(2@↑{h}@(minus)2)}
distributions with one peg empty.
@nb{The total} number of distributions is 3@↑{h}. Hence there are
@nb{3@↑{h}@(minus)3(2@↑{h}@(minus)2)@(minus)3} distributions without empty peg.
Contemplating separately all distributions for h=1 and h=2,
the formulas appear to be correct for these two cases too,
although this does not follow from the above deduction.

@bold{@larger{@larger{The end}}}
@(collect-garbage)
