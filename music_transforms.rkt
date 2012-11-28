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
;  (transpose phrase offset)
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
;(define trpnotes (transpose notes 60))
;(define slownotes (change-tempo trpnotes 1/2))


