#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require web-server/servlet web-server/servlet-env)
(require "views.rkt")
(require "models.rkt")

(provide route)

; routes
(define-values (route app-url)
  (dispatch-rules
    (("") a/new-tourney)
    (("t" "create") a/create-tourney)
    (("t" (string-arg)) a/show-tourney)
    (("t" (string-arg) "subscribe") a/subscribe)
    (else a/list-tourneys)))

; actions
(define (a/create-tourney request)
  (if (bytes=? #"POST" (request-method request))
    (let ((bindings (request-bindings request)))
      (if (tourney/save (bindings->tourney bindings))
        (a/show-tourney request (extract-binding/single 'id bindings))
        (v/internal-error)))
    (v/not-found)))

(define (a/list-tourneys request)
  (v/not-found))

(define (a/subscribe request id)
  (cond
    ((bytes=? #"GET" (request-method request))
     (let ((tourney (tourney/find id)))
       (if tourney
         (v/subscribe tourney (app-url a/subscribe id))
         (v/not-found))))
    ((bytes=? #"POST" (request-method request))
     (let ((tourney (tourney/find id)))
       (if tourney
         (let ((bindings (request-bindings request)))
           (let ((new-tourney
                   (tourney/append-member tourney (bindings->member bindings))))
             (if (tourney/save new-tourney)
               (v/show-tourney new-tourney (app-url a/subscribe id))
               (v/internal-error))))
         (v/not-found))))
    (else (v/not-found))))

(define (a/show-tourney request id)
  (let ((tourney (tourney/find id)))
    (if tourney
      (v/show-tourney tourney (app-url a/subscribe id))
      (v/not-found))))

(define (a/new-tourney request)
  (v/new-tourney (app-url a/create-tourney)))
