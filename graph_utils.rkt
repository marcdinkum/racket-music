#lang racket

; procedures to export
(provide validate-graph)
(provide remove-comment-from-line)
(provide remove-comments-from-multiline) ; input filter
(provide remove-junk-from-string) ;input filter
(provide make-length-ubiquitous) ; input filter

; ------------------------------------------------------
; ---------------- graph validator ---------------------
; ------------------------------------------------------

; Find a node in a graph by matching its key.
; Every node starts with its key.
; If the key is found, return the entire node (i.e. car lst), else #f.
(define (find-node key lst)
  ;(display (format "find ~a in ~a" key lst))(newline)(newline)
  (if (empty? lst) #f ; not found
    (if (equal? key (car (car lst))) (car lst)
        (find-node key (cdr lst)))))


; validating a graph:
; 1. make sure that every key has at least one successor.
;    For every node make sure that the key is followed by at least one element
; 2. make sure that every successor also has a successor
;    For every node, call find-node on all node elements in (cdr node) ---> this is the 'else' if pass one succeeded
; 3. make sure no key is used twice --not important-- because find-node comes back with the first occurrence
; Make sure to use the ORIGINAL graph for find-node !

(define (validate-graph org-graph) ; a-graph is the graph to validate
  (define (validate-subgraph a-graph)
  ; check-node calls find-node for every element to find out whether it
  ;  has a successor
  (define (check-node node)
   (if (empty? node) #t
     (if (eq? (find-node (car node) org-graph) #f) #f
       (check-node (cdr node)))))
  ; start checking
  (if (empty? a-graph) #t
    ; 1. make sure that every key has at least one successor
    ; (car a-graph) refers to the first node of the rest of the original graph
    (if (< (length (car a-graph)) 2) (format "Element ~a heeft geen opvolgers" (car (car a-graph)))
      ; 2. make sure that every successor also has a successor
      ; (cdr (car a-graph)) refers to the cdr of the first node of the rest of the original graph
      (if (eq? (check-node (cdr (car a-graph))) #f) (format "Node ~a heeft elementen zonder opvolgers" (car a-graph))
        ; else continue with the rest of the graph
        (validate-subgraph (cdr a-graph))))))
  (validate-subgraph org-graph))

; ---------------------------------------------------
; ------------------- input filter ------------------
; ---------------------------------------------------
;
; The input filter helps to clean up a Lilypond specification of note
; material by providing several procedures for a step-by-step cleanup:
; - comment remover
; - junk remove procedure (with user-specified  strings to remove)
; - a procedure that repeats note length for notes that don't have it specified


; remove-comment-from-line removes comment from a line given as a string in str.
; The comment as indicated by comment-string is removed from str, as is the rest of the line.
;
; The return value is a string, which can be empty
(define (remove-comment-from-line str comment-string)
  (let ((splitresult (string-split str comment-string)))
    (if (= (length splitresult) 0) ""
      (car (string-split str comment-string)))))


; remove-comment-from-linelist removes all single-line comments from every
;  line in the given list of lines
; The comment as indicated by comment-string is removed, as is the rest of that line
;
; The return value is a string containing one or more newline-separated
;  lines with their comments removed
(define (remove-comment-from-linelist lst comment-string)
  (if (empty? lst) ""
    (string-append (remove-comment-from-line (car lst) comment-string)
                   (remove-comment-from-linelist (cdr lst) comment-string))))


; remove-comments-from-multiline removes all single-line comments from a
;  string containing one or more lines separated by newline characters.
;
; The return value is a string containing one or more newline-separated
;  lines with their comments removed
(define (remove-comments-from-multiline str comment-string)
  (remove-comment-from-linelist (string-split str "\n") comment-string))


; recursively apply string-replace to the input string, every time replacing one of the junk-strings by nothing
(define (remove-junk-from-string inputstring lst)
  (if (empty? lst) inputstring
    (string-replace (remove-junk-from-string inputstring (cdr lst)) (car lst) "")))


; use a regexp to get the alpha part of a complex note symbol
; This is a modified version of get-note-pitch. This one does NOT convert back to a symbol
; N.B.: regexp returns a list. We return the first element
(define (get-pitch-string complex-note)
  (car (regexp-match #px"[[:alpha:]]+" complex-note)))
;(regexp-match #px"[[:alpha:]]+" (symbol->string complex-note)))


; use a regexp to get the numeric part of a complex note symbol
; This is a modified version of get-note-length. This one does NOT convert back to a symbol
; N.B.: regexp returns a list. We return the first element if not #f, else #f
(define (get-length-string complex-note)
  (define result (regexp-match #px"[[:digit:]]+" complex-note))
    (if (eq? result #f) #f
        (car result)))
;   (regexp-match #px"[[:digit:]]+" (symbol->string complex-note)))


; join pitch and length strings into a symbol
(define (reconstruct-note-symbol pitch length)
  (string->symbol (string-append pitch length)))


; make-length-ubiquitous repeats the last given note length value until it gets a new one
(define (make-length-ubiquitous lst current-length)
  (if (empty? lst) '()
      ; if next symbol has no specified length use the old value
    (if (eq? (get-length-string (car lst)) #f)
      (cons (reconstruct-note-symbol (get-pitch-string (car lst)) current-length) (make-length-ubiquitous (cdr lst) current-length))
    ; else
      (cons (reconstruct-note-symbol (get-pitch-string (car lst)) (get-length-string (car lst))) (make-length-ubiquitous (cdr lst) (get-length-string (car lst)))))))


; Examples of the input filter
;
; remove all single-line comments
;(define no-comment (remove-comments-from-multiline original-notes "%"))
;
; remove all unwanted ornaments
;(define junk-strings '("'"  ","   "."  "|"  "{"  "}"  "~"  "%"  "\n"))
;(define filtered-input (remove-junk-from-string no-comment junk-strings))
;
; give every note a length value
;(define prepared-notes (make-length-ubiquitous (string-split filtered-input) 1))


