;********************************************************************
;       (c) Copyright 2013, Hogeschool voor de Kunsten Utrecht
;                       Hilversum, the Netherlands
;********************************************************************
;
; File name	: lilypond.rkt
; System name	: SCEME: Scheme Music Composition Environment
;
; Description   : Racket Scheme class for creating Lilypond files
;
; Authors       : Marc Groenewegen, Daan van Hasselt
; E-mail        : marc.groenewegen@kmt.hku.nl, daan.vanhasselt@kmt.hku.nl
;
;*********************************************************************
;
; Class and convenience functions for writing Lilypond files
;
; Examples:
;  at end of file
;
;*********************************************************************

#lang racket

(provide make-lilypond-file) ; kept for backwards compatibility
(provide begin-nieuwe-lilypond-file)
(provide lilypond-titel)
(provide lilypond-componist)
(provide lilypond-tempo)
(provide lilypond-sleutel)
(provide lilypond-schaal)
(provide lilypond-instrument)
(provide schrijf-lilypond-file)


; create a lilygenerator so the user doesn't need to specify it with
;  every function call
(define lilygenerator #f)


(define LilyGenerator%
 (class object%

  ; define some default values
  (define fileport 0)
  (define file_open #f)
  (define title "Titel")
  (define composer "Componist")
  (define tempo 120)
  (define key "c")
  (define keytype "major")
  (define instrument-specified #f)
  (define instrument-name "violin")
  (define instrument "violin")
  (define keynumber 0)

  (define major-scales (list
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)))

  (define minor-scales (list
  '(c des d es e f ges g as a bes b)
  '(bis cis d dis e f fis g gis a ais b)
  '(c des d es fes f ges g as beses bes ces)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes ces)
  '(c cis d dis e f fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(c des eses es fes f ges g as beses bes ces)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)
  '(c des d es fes f ges g as a bes ces)
  '(c cis d dis e f fis g gis a ais b)
  '(bis cis d dis e eis fis g gis a ais b)
  '(c des d es e f ges g as a bes b)
  '(c cis d dis e f fis g gis a ais b)))


   (define base-notes (vector "c" "cis" "des" "d" "dis" "es" "e" "f" "fis"
                           "ges" "g" "gis" "as" "a" "ais" "bes" "b"))

  (super-new)

  (define/public (openFile filename)
   (set! fileport (open-output-file filename #:exists 'replace)))

  (define/public (set-title newtitle)
   (set! title newtitle))

  (define/public (set-composer new-composer)
   (set! composer new-composer))

  (define/public (set-tempo new-tempo)
   (set! tempo new-tempo))

  (define/public (set-key new-key)
   (set! key (string-downcase new-key))
   (set! keynumber (vector-member key base-notes)))

  (define/public (set-keytype new-keytype)
   (set! keytype new-keytype))

  (define/public (set-instrument new-instrument-name new-instrument)
   (set! instrument-name new-instrument-name)
   (set! instrument new-instrument)
   (set! instrument-specified #t))

  (define/public (write-header)
   (fprintf fileport
"\\version ~s
\\header {
  title = ~s
  composer = ~s
}\n\n" "2.10.5" title composer))


  (define/public (write-lily-file notes)
   (begin
    (fprintf fileport
"\\score
{
  \\new Staff
  {
    \\tempo 4=~a
    \\key ~a \\~a
    \\clef treble\n" tempo key keytype)
    (when instrument-specified
      (fprintf fileport
        (format "\\set Staff.instrumentName = \"~a\"\n" instrument-name))
      (fprintf fileport
        (format "\\set Staff.midiInstrument = #\"~a\"\n" instrument)))
 (parse notes) 
 (fprintf fileport
    "
    }
    \\layout {
       \\context { \\RemoveEmptyStaffContext }
    }
    \\midi { }
  }" )))
 
  ; convert a midi note number into a note name (primitive version)
  ;
  ; should be able to:
  ; * handle symbolic note names as well as numbers
  ; * use modal correction
  (define/private (number-to-note number)
     (if (string=? keytype "major")
       (list-ref (list-ref major-scales keynumber) (modulo number 12))
       (list-ref (list-ref minor-scales keynumber) (modulo number 12))))


  ;; relative to lilypond's middle C !!
  (define/private (number-to-octave number)
    (- (floor (/ number 12)) 4))

  ; If length is an exact integer, leave it as is it. If not, assume the
  ;  note is dotted, which means it is 3/2 times as long as the integer.
  ; Since this has already been discounted we need to nullify it and add a
  ;  dot to accord with Lilypond's notation
  (define/private (length-encoding number)
    (if (exact-integer? number) number
      (format "~a." (* 3/2 number))))


  ;; Lilypond uses single quote and comma to raise or lower a note's pitch by
  ;;  one octave
  ;; (number-to-quotes) takes a number and returns a string
  ;;  number==0 --> empty string
  ;;  number>0 --> string containing <number> quotes
  ;;  number<0 --> string containing <number> commas
  (define (number-to-quotes number)
    (cond ((= number 0) "")
          ((> number 0) (string-append "'" (number-to-quotes (- number 1))))
          ((< number 0) (string-append "," (number-to-quotes (+ number 1))))))

  ; display a single note
  (define/private (display-note note)
    (fprintf fileport "~a~a~a "
      (number-to-note (cadr note)) ;; note name
      (number-to-quotes (number-to-octave (cadr note))) ;; quotes for octave
      (length-encoding (caddr note)))) ;; note length

  ; display a rest (a.k.a. nap)
  (define/private (display-nap nap)
    (fprintf fileport "r~a " (length-encoding (cadr nap))))

  ; parse a composition
  (define/private (parse item)
    ; the first element of the item we're parsing is always a keyword
    (define keyword (car item))
    (cond 
      ((equal? keyword 'serial) ; the start of a serial block
       (fprintf fileport "{ "); the start of a serial block
       (for ((i (cdr item))); parse the rest of the items
	     (parse i))
       (fprintf fileport "} \\\\ \n")); the end of a serial block
       ; double backslash creates a new voice for every block

      ((equal? keyword 'parallel) 
       (fprintf fileport "<< "); the start of a parallel block
       (for ((i (cdr item))); parse the rest of the items
	     (parse i))
       (fprintf fileport ">> ")); the end of parallel
      
      ; notes and naps should just be displayed
      ((equal? keyword 'note)
	 (display-note item))
      ((equal? keyword 'nap)
	 (display-nap item)))) ;; end parse

  (define/public (closeFile)
    (close-output-port fileport))

)) ; class LilyGenerator%


; show warning if functions are called on non-existent object
(define (warning-no-lily-object)
  (display "Begin eerst een nieuwe file met begin-nieuwe-lilypond-file\n"))

(define (begin-nieuwe-lilypond-file filename)
  (set! lilygenerator (new LilyGenerator%))
  (send lilygenerator openFile filename))
  
(define (lilypond-titel title)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-title title)))

(define (lilypond-componist composer)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-composer composer)))

(define (lilypond-tempo tempo)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-tempo tempo)))

(define (lilypond-sleutel key)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-key key)))

(define (lilypond-schaal keytype)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-keytype keytype)))

(define (lilypond-instrument instrument-name instrument)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (send lilygenerator set-instrument instrument-name instrument)))

(define (schrijf-lilypond-file notes)
  (if (equal? lilygenerator #f)
    (warning-no-lily-object)
    (begin
      (send lilygenerator write-header)
      (send lilygenerator write-lily-file notes)
      (send lilygenerator closeFile)
      (set! lilygenerator #f)))) ; instruct garbage collector to destroy the object


; for backwards compatibility: the all-in-one lilypond construction call
(define (make-lilypond-file filename title composer key keytype notes)
   (begin
    (set! lilygenerator (new LilyGenerator%))
    (send lilygenerator openFile filename)
    (send lilygenerator set-title title)
    (send lilygenerator set-composer composer)
    (send lilygenerator set-key key)
    (send lilygenerator set-keytype keytype)
    (send lilygenerator write-header)
    (send lilygenerator write-lily-file notes)
    (send lilygenerator closeFile)
    (set! lilygenerator #f))) ; instruct garbage collector to destroy the object


; Examples
;
; (define notes '(serial
;  (note 61 8) (note 60 8) (note 67 8) (note 69 8) (note 70 8)
;  (note 70 8) (note 69 8) (note 70 8) (note 67 8) (note 63 8)
;  (note 65 8) (note 67 8) (note 63 8) (note 62 8) (note 62 8)
;  (note 60 8))
;
; The elaborate way: open a file, specify some props and write it
; (begin-nieuwe-lilypond-file "example_1.ly")
; (lilypond-titel "Een en al vrolijkheid")
; (lilypond-componist "Marc")
; (lilypond-tempo 78)
; (lilypond-sleutel "g")
; (lilypond-schaal "minor")
; (lilypond-instrument "guitar" "acoustic guitar (nylon)")
; (schrijf-lilypond-file notes)
;
; The compact way: everything in one function call
; (make-lilypond-file "example_2.ly" "Just a song" "Somebody" "c" "major" notes)

