#lang racket

(provide start-listening); Start listening to incoming events
(provide set-receive-bpm); Set a tempo in BPM
(provide set-silence-timeout) ; Set the minimum amount of time to detect silence, in ms
(provide set-note-callback); Set a function that will be called with a list of notes when silence is detected
(provide set-receive-port); Set a port to receive OSC on
(provide set-receive-host); Set a host to receive OSC on

(provide remove-item-from-list) ; Remove an item at an index from a list

(require racket/udp
         inf/opensoundcontrol/osc-to-bytes
         inf/opensoundcontrol/osc-defns
         inf/opensoundcontrol/bytes-to-osc)

; Enable log messages
(define debug #f)

; The currently active pitch
(define currently-active-note -1)

; Set a port to receive OSC on
(define port 7771)
(define (set-receive-port new-port)
  (set! port new-port))

; Set a host to receive OSC on
(define host #f)
(define (set-receive-host new-host)
  (set! host new-host))

(define receive-socket (udp-open-socket))
(define receive-buffer (make-bytes 10000 0))
(udp-bind! receive-socket host port)

; Set a function that will be called with a list of notes when silence is detected
(define note-callback (lambda (notes) 
                        (when debug
                          (display notes)(newline))
                        (display "Please set a note-callback!")(newline)))
(define (set-note-callback callback)
  (set! note-callback callback))

; Set a tempo in BPM
(define bpm 120)
(define (set-receive-bpm new-tempo)
  (set! bpm new-tempo))

; Set the minimum amount of time to detect silence, in ms
(define silence-timeout 5000)
(define (set-silence-timeout new-silence-timeout)
  (set! silence-timeout new-silence-timeout))

; This list will hold all events
(define event-list '())

; Round a number to the next power of two
; http://stackoverflow.com/a/466256
(define (next-power-of-two num)
  (expt 2 (ceiling (/ (log num) (log 2)))))

; Convert a time value in ms into a musical time value using a BPM
(define (ms->musicaltime ms bpm)
  (let* ((whole-duration (/ 240 (/ bpm 1000)))
         (note-fraction (/ whole-duration ms))
         (rounded-duration (next-power-of-two note-fraction)))
    (max rounded-duration 1)))
    
; Convert a midi pitch into a note name
(define (number-to-note number)
 (let* ((note-names '(c cis d dis e f fis g gis a ais b))
        (note-index (modulo number (length note-names))))
   (list-ref note-names note-index)))
    
; Create a note in the format 'c8 from a note-on and a note-off event
(define (create-note-from-events noteon-event noteoff-event)
  (let* ((note-name (number-to-note (cadr noteon-event)))
         (tOn (caddr noteon-event))
         (tOff (caddr noteoff-event))
         (deltaT (- tOff tOn))
         (note-duration (ms->musicaltime deltaT bpm)))
    (string->symbol (format "~a~a" note-name (inexact->exact note-duration)))))

; Find the first note-off event for a given pitch
; Returns two values, the event that is found and the index at which it is found
; Sets both values to #f if no event is found
(define (get-first-note-off pitch events [index 0]) ; (check out this optional argument magic!)
  (if (empty? events)
      (values #f #f)
      (if (= (cadr (car events)) pitch)
          (values (car events) index)
          (get-first-note-off pitch (cdr events) (+ index 1)))))

; Removes an item at an index from a list
(define (remove-item-from-list lst index)
  (let ((remaining-items (append (take lst index) (list-tail lst (+ index 1)))))
    (if (not remaining-items)
        '()
        remaining-items)))
  

; For the first event in the list, find its counterpart (eg find the first note-off with the same pitch as the first event)
; Return a note in the format 'c8 and the list of remaining events
(define (get-first-note-from-events events)
  (if (or (not (list? events)) (empty? events))
      (values #f '())
      (begin
        (let ((note-on (car events)))
          (let-values (((note-off-event note-off-index) (get-first-note-off (cadr note-on) (cdr events))))
            (if (not note-off-event)
                (begin
                  (when debug
                    (display (format "Error: could not find a note off for the note-on event ~a" note-on))(newline))
                  (values #f (cdr events)))
                (let ((remaining-events (remove-item-from-list (cdr events) note-off-index)))
                  (values (create-note-from-events note-on note-off-event) remaining-events))))))))

; Convert a list of events into a list of notes to be used by the graph generator
(define (convert-events-to-notes events)
  (if (empty? events)
      '()
      (let-values (((note remaining-events) (get-first-note-from-events events)))
        (if (not note)
            (convert-events-to-notes remaining-events)
            (cons note (convert-events-to-notes remaining-events))))))

; Create an event from an osc address and osc arguments
; Events have the format '(<address> <pitch> <time in ms>)
(define (create-event osc-address osc-args)
  (list osc-address (car osc-args) (current-milliseconds)))

; Add an event to the events-list
(define (add-event event)
  
  ; if this a note-off for the currently active note, reset the currently active note to -1
  (when (and (= (cadr event) currently-active-note) (equal? (car event) #"/noteoff"))
    (set! currently-active-note -1))
          
  ; if this a new note-on while the other note was still playing, insert a note off
  (when (and (and (not (= currently-active-note -1)) (not (= (cadr event) currently-active-note))) (equal? (car event) #"/noteon"))
      (add-event (list #"/noteoff" currently-active-note (current-milliseconds))))
    
  ; if this a note-off for another note then the currently active note, ignore it
  (unless (and (and (not (= currently-active-note -1)) (not (= (cadr event) currently-active-note))) (equal? (car event) #"/noteoff"))
    (set! event-list (append event-list (list event)))))

; Detect silence in the incoming stream of events
(define last-note-time 0)
(define is-silent #t)

; The function called when silence is detected
(define (silence-handler)
  (set! is-silent #t)
  (note-callback (convert-events-to-notes event-list))
  (set! event-list '()))

(define (start-listening)
  ; Listen for silence
  (thread
   (lambda ()
     (let detect-silence ()
       (when (and (not is-silent) (> (- (current-milliseconds) last-note-time) silence-timeout))
         (silence-handler))
       (sleep 0.001)
       (detect-silence))))

  ; Wait for incoming OSC messages
  ;
  ; osc-message is a struct definition containing address and args
  ;  elements are accessed using (osc-message-address <some-osc-msg>) to retrieve the address and
  ;  (osc-message-args <some-osc-msg>) for the arguments
  ;
  (thread
   (lambda ()
     (let loop ()
       ; udp-receive! returns 3 values: number of bytes, hostname and source port
       (define-values (nrofbytes hostname src-port) (udp-receive! receive-socket receive-buffer))
       (define received (subbytes receive-buffer 0 nrofbytes))
       (define decoded-message (bytes->osc-element received))
       
       ; set our last-note-time and add an event
       (set! last-note-time (current-milliseconds))
       (set! is-silent #f)
       (add-event (create-event (osc-message-address decoded-message) (osc-message-args decoded-message)))
     
       (when debug
         (printf "Received message: ~a args ~a\n" (osc-message-address decoded-message) (osc-message-args decoded-message)))
       
       (sleep 0.001)
       (loop))))
  (display (format "Started listening for incoming events on port ~a~n" port)))
