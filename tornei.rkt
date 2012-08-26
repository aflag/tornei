#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require web-server/servlet web-server/servlet-env)
(require file/md5)

; main layout
(define (layout . body)
  (response/xexpr
    `(html
      (head (title "Fazedor de torneios"))
      (body ,@body))))

; views
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
      (p "Senha: " (input ((type "text") (name "pass"))))
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

; actions
(define (a/create-tourney request)
  (if (bytes=? #"POST" (request-method request))
    (let ((bindings (request-bindings request)))
      (if (tourney/save (bindings->tourney bindings))
        (a/show-tourney request (extract-binding/single 'id bindings))
        (v/internal-error)))
    (v/not-found)))

(define (a/list-tourneys request)
  (layout '(h1 "Lista de torneios")))

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

; models
(define tourney/path
  (string-append (path->string (current-directory)) "t/"))

(define (bindings->assoc bindings)
  (map (lambda (v) (list (car v) (cdr v))) bindings))

(define (bindings->member bindings)
  (let ((pass->md5
          (lambda (fields)
            (map (lambda (item)
                   (if (symbol=? 'pass (car item))
                     (list 'pass (md5 (cadr item)))
                     item))
                 fields))))
    ; change password to md5sum
    (let ((fields (pass->md5 (bindings->assoc bindings))))
      ; create a member entry with the format (id fields), where id is a symbol
      ; and fields a list of memeber fields (like password, name, etc).
      (let ((id-item (assoc 'id fields)))
        (list (string->symbol (cadr id-item)) fields)))))

(define (bindings->tourney bindings)
  (bindings->assoc bindings))

(define (tourney/save tourney)
  (if tourney
    (let ((id (tourney/get 'id tourney)))
      (if id
        (write-to-file
          tourney
          (string-append tourney/path id)
          #:mode 'binary
          #:exists 'truncate/replace)
        #f))
    #f))

(define (tourney/get attr tourney)
  (let ((default-value
          (lambda (attr)
            (cond
              ((symbol=? attr 'members) '())
              (else #f)))))
    (let ((value (assoc attr tourney)))
      (if value (cadr value) (default-value attr)))))

(define (tourney/has? attr tourney)
  (if (assoc attr tourney) #t #f))

(define (tourney/find id)
  (let
    ((data-read
       (lambda (file)
         (if (file-exists? file)
           (file->value file)
           #f))))
    (data-read (string-append tourney/path id))))

(define (tourney/append-member tourney tourney-member)
  (let ((old-members (tourney/get 'members tourney)))
    (if (not (tourney/has? 'members tourney))
      (cons `(members ,(list tourney-member)) tourney)
      (if (assoc (car tourney-member) old-members)
        #f
        (map (lambda (item)
               (if (symbol=? (car item) 'members)
                 (list 'members (cons tourney-member old-members))
                 item))
             tourney)))))

; routes
(define-values (route app-url)
  (dispatch-rules
    (("") a/new-tourney)
    (("t" "create") a/create-tourney)
    (("t" (string-arg)) a/show-tourney)
    (("t" (string-arg) "subscribe") a/subscribe)
    (else a/list-tourneys)))

; boiler-plate
(define (start request)
  (route request))

(serve/servlet
  start
  #:port 8000
  #:servlet-path ""
  #:servlet-regexp #rx"")
