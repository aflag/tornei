#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require web-server/servlet web-server/servlet-env)
(require "models.rkt")

(provide
  v/new-tourney
  v/show-tourney
  v/subscribe
  v/internal-error
  v/not-found)

(define (layout . body)
  (response/xexpr
    `(html
      (head (title "Fazedor de torneios"))
      (body ,@body))))

(define (v/new-tourney action-url)
  (layout
    '(h1 "Fazedor de torneio")
    `(form ((method "post") (action ,action-url))
      (p "Nome do torneio: " (input ((type "text") (name "name"))))
      (p "URL: tornei.kontesti.me/t/" (input ((type "text") (name "id"))))
      (p (input ((type "submit") (value "Registrar")))))))

(define (v/show-tourney tourney subscribe-url)
  (layout
    `(h1 ,(tourney/get 'name tourney))
    `(a ((href ,subscribe-url)) "inscrever")
    '(h2 "Membros")
    `(ul ,@(map
            (lambda (item)
              (list 'li (symbol->string (car item))))
            (tourney/get 'members tourney)))))

(define (v/subscribe tourney action-url)
  (layout
    '(h1 "Inscrever")
    `(form ((action ,action-url) (method "post"))
      (p "Nick: " (input ((type "text") (name "id"))))
      (p "Senha: " (input ((type "password") (name "pass"))))
      (input ((type "submit" (name "inscrever")))))))

(define (v/internal-error)
  (response/full
    500
    #"Internal Error"
    (current-seconds)
    TEXT/HTML-MIME-TYPE
    '()
    '(#"<html><body><h1>Internal error!</h1></body></html>")))

(define (v/not-found)
  (response/full
    404
    #"Not found"
    (current-seconds)
    TEXT/HTML-MIME-TYPE
    '()
    '(#"<html><body><h1>Not found!</h1></body></html>")))
