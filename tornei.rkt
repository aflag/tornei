#lang racket

(require web-server/servlet web-server/servlet-env)

; main layout
(define (layout . body)
  (response/xexpr
    `(html
      (head (title "Tornei"))
      (body ,@body))))

; views
(define (v-new-tourney action-url)
  (layout
    '(h1 "Fazedor de torneio")
    `(form ((method "post") (action ,action-url))
      (p "Nome do torneio: " (input ((name "name"))))
      (p "URL: tornei.kontesti.me/t/" (input ((name "id"))))
      (p (input ((type "submit") (value "Registrar")))))))

(define (v-show-tourney tourney)
  (layout
    `(h1 ,(tourney-get 'name tourney))))

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
  (layout '(h1 "List!")))

(define (show-tourney request id)
  (let ((tourney (find-tourney id)))
    (if tourney
      (v-show-tourney tourney)
      (v-not-found))))

(define (new-tourney request)
  (let
    ((response-generator
      (lambda (embed/url)
        (v-new-tourney (embed/url create-tourney)))))
    (send/suspend/dispatch response-generator)))

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
(define-values (route base-url)
  (dispatch-rules
    (("") new-tourney)
    (("t" (string-arg)) show-tourney)
    (else list-tourneys)))

; boiler-plate
(define (start request)
  (route request))

(serve/servlet
  start
  #:port 8000
  #:servlet-path ""
  #:servlet-regexp #rx"")