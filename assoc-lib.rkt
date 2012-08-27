#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(provide
  assoc-value)

(define (assoc-value attr assoc-list)
  (let ((item (assoc attr assoc-list)))
    (and item (cadr item))))
