#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require web-server/servlet web-server/servlet-env)
(require "actions.rkt")

(define (start request)
  (route request))

(serve/servlet
  start
  #:port 8000
  #:servlet-path ""
  #:servlet-regexp #rx"")
