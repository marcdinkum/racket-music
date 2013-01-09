;********************************************************************
;       (c) Copyright 2013, Hogeschool voor de Kunsten Utrecht
;                       Hilversum, the Netherlands
;********************************************************************
;
; File name	: osc.rkt
; System name	: SCEME: Scheme Music Composition Environment
;
; Description   : Racket Scheme class for OSC communication
;
; Authors       : Marc Groenewegen, Daan van Hasselt
; E-mail        : marc.groenewegen@kmt.hku.nl, daan.vanhasselt@kmt.hku.nl
;
;*********************************************************************
;
; Class and convenience functions for OSC communication
;
; Exports procedures:
;  (osc-open host-address port)
;  (osc-send-msg message)
;  (osc-close)
;
; Examples:
;  at end of file
;
;*********************************************************************

#lang racket

(require racket/udp)

(provide osc-open)
(provide osc-send-msg)
;(provide osc-close)


; create placeholder for a new osc-bridge
(define osc-bridge null)


(define OSC%
 (class object%

  ; create a socket
  (define clientsocket (udp-open-socket))
  (define port-open #f)

  (super-new)

  ; zero padding if #bytes%4 not 0
  (define/private (pad4 bytestring)
    (if (= (modulo (bytes-length bytestring) 4) 0) bytestring
     (pad4 (bytes-append bytestring (bytes 0)))))

  ; integer->integer-bytes converts integer n to byte string of given length (2, 4 or 8)
  ; extra parameter: signed
  (define/private (toOSCint32 arg) ;; int32 big endian
   (integer->integer-bytes arg 4 #t #t))

  ; construct OSC message from its address and payload given the typespec
  ;
  ; bytes-append* expects a list as last parameter. We use this for handing it the values
  ;  that the user wants to send
  ; Supported types:
  ;  32 bit int
  ;  string (we need to append a \0 before applying pad4, as Racket does not automatically do this)
  ; TODO : float
  ;
  ; Between the address and the typespec is a comma ','. For padding this is part of the typespec.
  ; The typespec needs a \0 at the end (before pad4).
  ; Zero padding is needed for the address, typespec (including comma) and all parameters that are
  ;   not automatically a multiple of 4 bytes
  ; Use the parameter's type to determine how to encode it
  (define/private (osc-msg address typespec payload)
    (define (typeconverter item)
      (cond ((number? item) (toOSCint32 item))
	    ((string? item)  (pad4 (bytes-append (string->bytes/latin-1 item) (bytes 0))))))
   (bytes-append* (pad4 (bytes-append #"/" (string->bytes/latin-1 address)))
	  (pad4 (bytes-append #"," (string->bytes/latin-1 typespec) (bytes 0)))
	  (for/list ((item payload)) (typeconverter item))))

  ; give the socket a destination (server/port)
  (define/public (open host-address port)
    (if port-open #f ; if port already open return false
      ; else open the port and mark it open
      (begin
        (udp-connect! clientsocket host-address port)
	(set! port-open #t))))

  ; send a message
  (define/public (send-msg address typespec payload)
    (udp-send clientsocket (osc-msg address typespec payload)))


)) ;; class


; Convenience functions for those that don't want to use the class directly
(define (osc-open host-address port)
  (begin
    (set! osc-bridge (new OSC%))
    (send osc-bridge open host-address port)))


(define (osc-send-msg address typespec payload)
  (send osc-bridge send-msg address typespec payload))


; Example: open new OSC port and send some data
; (osc-open "localhost" 7770)
; (osc-send-msg "racket" "iisi" '(17 400 "haha" 21))

