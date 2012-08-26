#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require web-server/servlet web-server/servlet-env)

; main layout
(define (layout . body)
  (response/xexpr
    `(html
      (head (title "Fazedor de torneios"))
      (body ,@body))))

; views
(define (v-new-tourney action-url)
  (layout
    '(h1 "Fazedor de torneio")
    `(form ((method "post") (action ,action-url))
      (p "Nome do torneio: " (input ((name "name"))))
      (p "URL: tornei.kontesti.me/t/" (input ((name "id"))))
      (p (input ((type "submit") (value "Registrar")))))))

(define (v-show-tourney tourney subscribe-url)
  (layout
    `(h1 ,(tourney-get 'name tourney))
    `(a ((href ,subscribe-url)) "inscrever")))

(define (v-internal-error)
  (response/full
    500
    #"Internal Error"
    (current-seconds)
    TEXT/HTML-MIME-TYPE
    '()
    '(#"<html><body><h1>Internal error!</h1></body></html>")))

(define (v-not-found)
  (response/full
    404
    #"Not found"
    (current-seconds)
    TEXT/HTML-MIME-TYPE
    '()
    '(#"<html><body><h1>Not found!</h1></body></html>")))

; actions
(define (create-tourney request)
  (if (bytes=? #"POST" (request-method request))
    (let ((bindings (request-bindings request)))
      (if (tourney-save (bindings->tourney bindings))
        (show-tourney request (extract-binding/single 'id bindings))
        (v-internal-error)))
    (v-not-found)))

(define (list-tourneys request)
  (layout '(h1 "Lista de torneios")))

(define (subscribe-tourney request id)
  (layout
    '(h1 "Inscrever")
    `(p "Tourney id: " ,id)))

(define (show-tourney request id)
  (let ((tourney (find-tourney id)))
    (if tourney
      (v-show-tourney tourney (app-url subscribe-tourney id))
      (v-not-found))))

(define (new-tourney request)
  (v-new-tourney (app-url create-tourney)))

; models
(define tourney-path
  (string-append (path->string (current-directory)) "t/"))

(define (bindings->tourney bindings)
  (map (lambda (v) (list (car v) (cdr v))) bindings))

(define (tourney-save tourney)
  (let ((id (tourney-get 'id tourney)))
    (if id
      (write-to-file
        tourney
        (string-append tourney-path id)
        #:mode 'binary
        #:exists 'truncate/replace)
      #f)))

(define (tourney-get attr tourney)
  (let ((value (assoc attr tourney)))
    (if value (cadr value) #f)))

(define (tourney-has? attr tourney)
  (if (assoc attr tourney) #t #f))

(define (find-tourney id)
  (let
    ((data-read
       (lambda (file)
         (if (file-exists? file)
           (file->value file)
           #f))))
    (data-read (string-append tourney-path id))))

; routes
(define-values (route app-url)
  (dispatch-rules
    (("") new-tourney)
    (("t" "create") create-tourney)
    (("t" (string-arg)) show-tourney)
    (("t" (string-arg) "subscribe") subscribe-tourney)
    (else list-tourneys)))

; boiler-plate
(define (start request)
  (route request))

(serve/servlet
  start
  #:port 8000
  #:servlet-path ""
  #:servlet-regexp #rx"")
