;********************************************************************
;       (c) Copyright 2012, Hogeschool voor de Kunsten Utrecht
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
; Exports procedures:
;  (make-lilypond-file filename title composer key keytype notes)
;
; Example:
;  at end of file
;
;*********************************************************************

#lang racket

(provide make-lilypond-file)

(define LilyGenerator%
 (class object%

  (define fileport 0)

  (super-new)

  (define/public (openFile filename)
   (set! fileport (open-output-file filename #:exists 'replace)))
 

  (define/public (lilyheader title composer)
   (fprintf fileport
"\\version ~s
\\header {
  title = ~s
  composer = ~s
}\n\n" "2.10.5" title composer))


  (define/public (lilyscore key keytype notes)
   (begin
    (fprintf fileport
"\\score
{
  \\new Staff
  {
    \\key ~a \\~a
    \\clef treble\n" key keytype)
;; temporary
;;(fprintf fileport "\\relative c'")
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
  ; * handle symbolic note names as well as numbers,
  ; * put notes in the correct octave
  ; * use modal correction
  (define/private (number-to-note number)
    (define noteNames '(c cis d dis e f fis g gis a ais b))
    (list-ref noteNames (modulo number 12)))

  ;; relative to lilypond's middle C !!
  (define/private (number-to-octave number)
    (- (floor (/ number 12)) 4))


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
      (caddr note))) ;; note length

  ; display a rest (a.k.a. nap)
  (define/private (display-nap nap)
    (fprintf fileport "r~a " (cadr nap)))

  ; parse a composition
  (define/private (parse item)
    ; the first element of the item we're parsing is always a keyword
    (define keyword (car item))
    (cond 
      ((equal? keyword 'serial) ; the start of a serial block
       (fprintf fileport "{ "); the start of a serial block
       (for ((i (cdr item))); parse the rest of the items
	     (parse i))
       (fprintf fileport "} ")); the end of a serial block

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

)) ;; class


(define (make-lilypond-file filename title composer key keytype notes)
  (define generator (new LilyGenerator%))
   (begin
     (send generator openFile filename)
     (send generator lilyheader title composer)
     (send generator lilyscore key keytype notes)
     (send generator closeFile)))
 
;; Example
; (define notes '(serial (note 60 4) (note 65 4) (nap 1) (parallel (note 60 4) (note 65 4))))
;
; (make-lilypond-file "test.ly" "Just a song" "Somebody" "c" "major" notes)

