#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require web-server/servlet web-server/servlet-env)
(require (for-syntax racket/syntax))
(require "views.rkt")
(require "models.rkt")

(provide route)

; routes
(define-values (route app-url)
  (dispatch-rules
    (("") a/create-tournament)
    (("t" "create") a/create-tournament)
    (("t" (string-arg)) a/show-tournament)
    (("t" (string-arg) "subscribe") a/subscribe)
    (("t" (string-arg) "start") a/start-tournament)
    (else a/list-tournaments)))

; macro helpers
(define-syntax request-handler
  (syntax-rules (h/get h/post)
    ((_ req) (v/not-found))
    ((_ req (h/get e1 e2 ...) r1 ...)
     (if (bytes=? #"GET" (request-method req))
       (begin e1 e2 ...)
       (request-handler req r1 ...)))
    ((_ req (h/post e1 e2 ...) r1 ...)
     (if (bytes=? #"POST" (request-method req))
       (begin e1 e2 ...)
       (request-handler req r1 ...)))))

(define-syntax (define-action stx)
  (syntax-case stx ()
    ((_ action-name code ...)
     (with-syntax 
       ((req (format-id #'action-name "req")))
       #`(define (action-name req)
	   (req-handler req code ...))))))

(define-syntax (define-action-with-tournament stx)
  (syntax-case stx ()
    ((_ action-name code ...)
     (with-syntax 
       ((tournament (format-id #'action-name "tournament"))
        (req (format-id #'action-name "req"))
        (id (format-id #'action-name "id")))
       #`(define (action-name req id)
            (let ((tournament (tournament/find id)))
              (if tournament
                (request-handler req code ...)
                (v/not-found))))))))

; actions
(define (a/list-tournaments req)
  (v/not-found))

(define (a/create-tournament req)
  (request-handler req
  (h/get
    (v/new-tournament (app-url a/create-tournament)))
  (h/post
    (let ((bindings (request-bindings req)))
      (if (tournament/save (bindings->tournament bindings))
	(redirect-to
	  (app-url a/show-tournament (extract-binding/single 'id bindings)))
	(v/internal-error))))))

(define-action-with-tournament a/start-tournament
  (h/get
    (v/start-tournament tournament (app-url a/start-tournament id)))
  (h/post
    (let ((bindings (request-bindings req)))
      (if (tournament/start tournament (bindings->assoc bindings))
	(redirect-to (app-url a/show-tournament  id))
	; bad password or something, reloading the same view again
	(v/start-tournament tournament (app-url a/start-tournament id))))))
	  

(define-action-with-tournament a/subscribe
  (h/get
    (v/subscribe tournament (app-url a/subscribe id)))
  (h/post
    (let ((bindings (request-bindings req)))
      (let ((new-tournament
	      (tournament/append-member tournament (bindings->member bindings))))
	(if (tournament/save new-tournament)
	  (v/show-tournament new-tournament (app-url a/subscribe id))
	  (v/internal-error))))))

(define-action-with-tournament a/show-tournament
  (h/get
    (v/show-tournament tournament (app-url a/subscribe id))))
