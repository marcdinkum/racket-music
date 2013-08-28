;********************************************************************
;       (c) Copyright 2012, Hogeschool voor de Kunsten Utrecht
;                       Hilversum, the Netherlands
;********************************************************************
;
; File name	: music_transforms.rkt
; System name	: SCEME: Scheme Music Composition Environment
;
; Description   : Racket Scheme functions for manipulating music data
;
; Authors       : Marc Groenewegen, Daan van Hasselt
; E-mail        : marc.groenewegen@kmt.hku.nl, daan.vanhasselt@kmt.hku.nl
;
;*********************************************************************
;
; Convenience functions for manipulating music data
;
; Exports procedures:
;  (make-phrase melody rhythm) - combine melody and rhythm into a phrase
;  (make-parallel phrase [phrase]+ ) - combine two or more phrases into a
;					parallel structure
;  (note-to-number note) - translate absolute note name to number
;  (notes-to-numbers lst) - translate list of absolute notes name to numbers
;  (transpose phrase offset)
;  (change-tempo lst factor) -- deprecated. Use scale-length instead
;  (scale-length lst factor)
;  (merge-phrases phrase1 phrase2 ....)
;  (merge-phraselist (phrase1 phrase2 ....))
;  (repeat-phrase phrase repeats)
;  (reverse-phrase phrase)
;  (apply-transform lst parallel-func serial-func note-func)
;
; Desired functions:
;  modulo12, erode, dilate, filter
;
; Examples: at end of file
;
;*********************************************************************

#lang racket

(provide make-phrase)
(provide make-parallel)
(provide note-to-number)
(provide notes-to-numbers)
(provide transpose)
(provide change-tempo)
(provide scale-length)
(provide merge-phrases)
(provide merge-phraselist)
(provide repeat-phrase)
(provide reverse-phrase)
(provide apply-transform)

; dotnote is used for dotted notes which have a duration of 3/2 times the
;  indicated integer.
(define (dotnote n)
  (if (flonum? n) (* 2/3 (inexact->exact n)) n))

; Combine melody and rhythm into a serial phrase according to our own format.
;
; A pitch identifier that is not a number is regarded as a nap.
;
; We don't want to support other notation like c, dis, bes here because
;  that would be interpreted as an absolute notation, which e.g. leads to
;  problems while transposing
; Well... maybe later if we fully understand the consequences :-)
(define (make-phrase melody rhythm)
  (cons 'serial
   (for/list ((note-pitch melody) (note-length rhythm))
     (if (number? note-pitch)
         (list 'note note-pitch (dotnote note-length))
         (list 'nap (dotnote note-length))))))

;;
;; make-parallel combines two or more voices into a parallel structure
;;
(define make-parallel (lambda phrases (cons 'parallel phrases)))


;;
;; translate absolute note name to number
;;
(define (note-to-number note)
 (let* ((raised-names (vector 'c 'cis 'd 'dis 'e 'f 'fis 'g 'gis 'a 'ais 'b))
         (lowered-names (vector 'c 'des 'd 'es 'e 'f 'ges 'g 'as 'a 'bes 'b))
         (result (vector-member note raised-names)))
   ;; if not found in the first vector the result is #f and we'll look in
   ;;   the second list
   (if (not result) (vector-member note lowered-names)
       result)))

;;
;; translate a list of absolute note names to numbers
;;
(define (notes-to-numbers lst)
  (for/list ((note lst)) (note-to-number note)))


;; transpose a note
;; first find out if note is a symbol like 'serial or 'parallel, in that case return unchanged
;;  else find out if note is a note or a nap or something else
;; if a note, transpose its pitch value and leave the rest unchanged
;; if a nap, return as-is
(define (transpose-note note offset)
  (if (symbol? note) note
    (if (equal? (car note) 'note) (list 'note (+ (cadr note) offset) (caddr note))
      note)))

; Transpose a phrase using apply-transform at note level
(define (transpose phrase offset)
  (apply-transform phrase '() '() (lambda (note) (transpose-note note offset))))


;; Scale the length of a note by a factor.
;; First find out if note is a symbol like 'serial or 'parallel, in that case return unchanged
;;  else change its length value and leave the rest unchanged
(define (scale-note-length note factor)
  (if (symbol? note) note
    (cond ((equal? (car note) 'note) (list (car note) (cadr note) (* (caddr note) factor)))
          ((equal? (car note) 'nap) (list (car note) (* (cadr note) factor))))))

;; Scale the lengths of all notes and rests in a phrase
;; 
(define (scale-length lst factor)
 (if (empty? lst) '()
    (cons (scale-note-length (car lst) factor) (scale-length (cdr lst) factor))))


; for the time being provide the old function for backwards compatibility
(define change-tempo scale-length)



; Merge two or more phrases into a single phrase.
;  merge-phrases takes a variable number of phrases as arguments
;  merge-phraselist takes a list of phrases in a single argument
;
; Explanation:
; flatten-phraselist is a helper function that strips the 'serial keyword
;  off every 'serial phrase
;
; lst is a list of phrases. Every phrase can start with 'serial or 'parallel
; Phrases starting with 'serial but the first one should be stripped of their keyword.
; Phrases starting with 'parallel should be inserted as-is
;
(define (flatten-phraselist lst)
  (if (empty? lst) '()
    (if (equal? (car (car lst)) 'serial) (append (cdr (car lst)) (flatten-phraselist (cdr lst)))
     (cons (car lst) (flatten-phraselist (cdr lst))))))

(define (merge-phraselist lst)
  (cons 'serial (flatten-phraselist lst)))

(define merge-phrases (lambda lst (merge-phraselist lst)))


; Repeat a phrase a number of times
; Use merge-phrases recursively using a helper function
; If given number of repeats <= 1, the original phrase is returned
;
(define (repeat-phrase phrase repeats)
  (define (repeat-phrase-helper currentPhrase currentRepeats)
    (if (<= currentRepeats 1) currentPhrase
      (repeat-phrase-helper (merge-phrases phrase currentPhrase) (- currentRepeats 1))))
   (repeat-phrase-helper phrase repeats))



; Reverse a phrase
;
(define (reverse-phrase phrase)
  (if (null? phrase)
      '()
      (if (equal? (car phrase) 'serial) ; currently only works with 'serial'
          (append '(serial) (reverse-phrase (cdr phrase)))
          (append (reverse-phrase (cdr phrase)) (list (car phrase))))))



; apply transformation functions to a list of notes
; this function takes a number of arguments:
; - lst  the list of notes to transform
; - parallel-func  the function to be applied to every parallel block
; - serial-func  the function to be applied to every serial block
; - note-func  the function to be applied to every note (or nap)

  (define (apply-transform lst parallel-func serial-func note-func)
    (cond 
      ; if the keyword is a list (so not a keyword) parse all the items in the current list
      ; do this using a for loop
      ((list? (car lst))
       (for/list ((i lst))
         (apply-transform i parallel-func serial-func note-func)))
      
      ; if this is a serial block, return '(serial . the transformed rest of the list)
      ((equal? (car lst) 'serial)
           (cons (car lst) (apply-transform (if (empty? serial-func) (cdr lst) (serial-func (cdr lst))) parallel-func serial-func note-func)))

      ; if this is a parallel block, return '(parallel . the transformed rest of the list)
      ((equal? (car lst) 'parallel)
       (cons (car lst) (apply-transform (if (empty? parallel-func) (cdr lst) (parallel-func (cdr lst))) parallel-func serial-func note-func)))
      
      ; if this is a note or a nap, return the note (transform if needed)
      ((or (equal? (car lst) 'note) (equal? (car lst) 'nap))
       (if (empty? note-func)
           lst
           (note-func lst)))))

  
;(define notes '(serial (note 0 4) (note 2 8) (note 4 8) (note 6 8)))

; transpose using note scope function
;(apply-transform notes 
;                 '()
;                 '()
;                 (lambda (lst) (list (car lst) (+ (cadr lst) 10) (caddr lst))))

; reverse using serial scope function
;(apply-transform notes 
;                 '()
;                 (lambda (lst) (reverse lst))
;                 '())

; change interval using parallel function
;(define notes '(parallel (note 0 4) (note 5 4)))

;(apply-transform notes
;                 (lambda (lst) (list 
;                                (list (car (car lst)) (- (cadr (car lst)) 1) (caddr (car lst))) 
;                                (list (car (cadr lst)) (+ (cadr (cadr lst)) 1) (caddr (cadr lst)))))
;                 '()
;                 '())

  
; change interval using note scope function in parallel scope function
;(define notes '(parallel (serial (note 0 4) (note 3 4)) (serial (note 5 4) (note 7 4))))

;(apply-transform notes
;                 (lambda (lst) (list 
;                                (apply-transform (car lst)
;                                                 '()
;                                                 '()
;                                                 (lambda (lst) (transpose-note lst -3)))
;                                (apply-transform (cadr lst)
;                                                 '()
;                                                 '()
;                                                 (lambda (lst) (transpose-note lst 3)))))
;                 '()
;                 '())
  
  
;; examples
;(define melodie '(9 7 5 4 7 nap 9 5))
;(define ritme '(16 16 16 16 16 16 16 16 16))
;(define notes (make-phrase melodie ritme))
;(define compositie (merge-phrases notes (transpose notes 3) (scale-length notes 2)))

;(define trpnotes (transpose notes 60))
;(define slownotes (scale-length trpnotes 1/2))
  
; merge trpnotes & slownotes to 1 phrase (and store it into variable bigphrase)
;(define bigphrase (merge-phrases trpnotes slownotes))
  
; reverse bigphrase (and store it into variable bigphrase-rev)
;(define bigphrase-rev (reverse-phrase bigphrase))
  
; merge a list of phrases (and store it into variable bigphrase2)
; (define bigphrase2 (merge-phraselist (list trpnotes slownotes bigphrase-rev slownotes bigphrase)))

