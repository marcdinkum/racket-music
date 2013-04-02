#lang racket

(provide set-play-port) ; Set the port to send notes to
(provide set-play-bpm) ; Set a tempo in BPM
(provide play-notes) ; Play a list of notes

(require racket/udp
         inf/opensoundcontrol/osc-to-bytes
         inf/opensoundcontrol/osc-defns
         inf/opensoundcontrol/bytes-to-osc
         inf/music_transforms)

(define host "127.0.0.1")
(define send-socket (udp-open-socket))

; Set a tempo in BPM
(define bpm 120)
(define (set-play-bpm new-tempo)
  (set! bpm new-tempo))

; Set the port to send notes to
(define port 7770)
(define (set-play-port new-port)
  (set! port new-port))

; This will hold all the events scheduled to be sent
(define event-list '())

; The start-time for the next event
(define next-event-time 0)

; Setup the scheduling loop
(thread
   (lambda ()
     (let loop ()
       (when (not (empty? event-list))
           (let* ((event (car event-list))
                  (event-time (caddr event)))
           (if (>= (current-milliseconds) event-time)
               (begin
                 (send-event event)
                 (set! event-list (cdr event-list)))
               (sleep 0.001))))
       (loop))))

; Send an OSC command
(define (send-command message)
  (udp-send-to send-socket host port (osc-element->bytes message)))
 
; Send an event
(define (send-event event)
  (if (equal? (car event) #"/noteon")
      (send-note-on (+ 60 (note-to-number (cadr event))))
      (send-note-off (+ 60 (note-to-number (cadr event))))))

; Send a note on
(define (send-note-on pitch)
  (send-command (osc-message #"/noteon" (list pitch))))

; Send a note off
(define (send-note-off pitch)
  (send-command (osc-message #"/noteoff" (list pitch))))
 
 ; use a regexp to get the alpha part of a complex note symbol
(define (get-note-pitch complex-note)
  (string->symbol (car (regexp-match #px"[[:alpha:]]+" (symbol->string complex-note)))))

; use a regexp to get the numeric part of a complex note symbol
(define (get-note-length complex-note)
   (string->number (car (regexp-match #px"[[:digit:]]+" (symbol->string complex-note)))))

; Convert a musical duration into a number of ms
(define (musicaltime->ms musicaltime bpm)
  (/ (/ 240000 bpm) musicaltime))

; Add a single event
(define (add-event event)
  (set! event-list (append event-list (list event))))

; Add a note-on and note-off event for a note
(define (add-events note)
  (let* ((pitch (get-note-pitch note))
        (musical-duration (get-note-length note))
        (ms-duration (musicaltime->ms musical-duration bpm))
        (noteoff-event-time (+ next-event-time ms-duration)))
    (add-event (list #"/noteon" pitch next-event-time))
    (add-event (list #"/noteoff" pitch noteoff-event-time))
    (set! next-event-time noteoff-event-time)))

; Sort all the events by timestamp
; This uses the 'insertion sort' algorithm
(define (sort-events events [sorted-events '()])
  (if (empty? events)
      sorted-events
      (sort-events (cdr events) (insert-sorted sorted-events (car events)))))

; Insert a item in a list, so that the resulting list is sorted by timestamp
(define (insert-sorted events event)
  (if (empty? events)
      (list event)
      (if (<= (caddr event) (caddr (car events)))
          (cons event events)
          (cons (car events) (insert-sorted (cdr events) event)))))

; Test the sorting
;(define test '(("/noteon" 60 0)("/noteoff" 60 500)("/noteon" 62 50)("/noteoff" 62 1000)))
;(sort-events test)

; Play a list of notes
; Every note is in the format 'd8
; For every note, add a note-on and note-off event. At the end, sort all events by their timestamp.
(define (play-notes notes)
  (set! next-event-time (current-milliseconds))
  (add-events-for-notes notes)
  (set! event-list (sort-events event-list)))

; Add events for all the notes in the list
(define (add-events-for-notes notes)
  (when (not (empty? notes))
        (add-events (car notes))
        (add-events-for-notes (cdr notes))))