;;
;; Class and convenience functions for writing Lilypond files
;;
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

  (define/private (number-to-quotes number)
    (define (generate-symbol number)
      (if (> number 0) "'" ","))
  
    (define (move-number-to-0 number)
      (if (> number 0) (- number 1) (+ number 1)))
  
    (if (= number 0) ""
        (string-append (generate-symbol number) (number-to-quotes (move-number-to-0 number)))))

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
 
