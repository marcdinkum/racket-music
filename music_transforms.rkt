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
;  (transpose phrase)
;  (change-tempo lst factor)
;  (merge-phrases phrase1 phrase2)
;  (repeat-phrase phrase repeats)
;
; Desired functions:
;  reverse, modulo12, erode, dilate, filter
;
; Examples: at end of file
;
;*********************************************************************

#lang racket

(provide make-phrase)
(provide transpose)
(provide change-tempo)
(provide merge-phrases)
(provide repeat-phrase)


;; combine melody and rhythm into a serial phrase according to our own format
(define (make-phrase melody rhythm)
  (cons 'serial
   (for/list ((note-pitch melody) (note-length rhythm))
     (if (number? note-pitch) (list 'note note-pitch note-length)
         (list 'nap note-length)))))


;; transpose a note
;; first find out if note is a symbol like 'serial or 'parallel, in that case return unchanged
;;  else find out if note is a note or a nap or something else
;; if a note, transpose its pitch value and leave the rest unchanged
;; if a nap, return as-is
(define (transpose-note note offset)
  (if (symbol? note) note
    (if (equal? (car note) 'note) (list 'note (+ (cadr note) offset) (caddr note))
      note)))


;; Transpose a phrase
;;
;; This procedure uses the transpose-note procedure to assess every element and if it is
;;  transpose-able perform the actual transpose
(define (transpose lst offset)
  (if (empty? lst) '()
    (cons (transpose-note (car lst) offset) (transpose (cdr lst) offset))))



;; change the tempo of a note
;; first find out if note is a symbol like 'serial or 'parallel, in that case return unchanged
;;  else change its tempo value and leave the rest unchanged
(define (change-note-tempo note factor)
  (if (symbol? note) note
    (cond ((equal? (car note) 'note) (list (car note) (cadr note) (* (caddr note) factor)))
          ((equal? (car note) 'nap) (list (car note) (* (cadr note) factor))))))

;; change the tempo of a phrase
;; 
(define (change-tempo lst factor)
 (if (empty? lst) '()
    (cons (change-note-tempo (car lst) factor) (change-tempo (cdr lst) factor))))



; merge two phrases into a single phrase
; a phrase starts with the keyword 'melody' or 'serial'
;
(define (merge-phrases phrase1 phrase2)
  (append phrase1 (cdr phrase2)))


; Repeat a phrase a number of times
; Use merge-phrases recursively using a helper function
; If given number of repeats <= 1, the original phrase is returned
;
(define (repeat-phrase phrase repeats)
  (define (repeat-phrase-helper currentPhrase currentRepeats)
    (if (<= currentRepeats 1) currentPhrase
      (repeat-phrase-helper (merge-phrases phrase currentPhrase) (- currentRepeats 1))))
   (repeat-phrase-helper phrase repeats))


;; examples
;(define melodie '(9 7 5 4 7 nap 9 5))
;(define ritme '(16 16 16 16 16 16 16 16 16))
;(define notes (make-phrase melodie ritme))
;(define trpnotes (transpose notes 60))
;(define slownotes (change-tempo trpnotes 1/2))


