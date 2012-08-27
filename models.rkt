#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require file/md5)
(provide
  bindings->assoc
  bindings->member
  bindings->tournament
  tournament/save
  tournament/get
  tournament/has?
  tournament/find
  tournament/append-member
  tournament/start)

(define tournament/path
  (string-append (path->string (current-directory)) "t/"))

(define (password->md5 fields)
  (let ((md5-if-pass (lambda (item)
                       (if (symbol=? 'pass (car item))
                         (list 'pass (md5 (cadr item)))
                         item))))
    (map md5-if-pass fields)))

(define (bindings->assoc bindings)
  (password->md5
    (map (lambda (v) (list (car v) (cdr v))) bindings)))

(define (bindings->member bindings)
    (let ((fields (bindings->assoc bindings)))
      ; create a member entry with the format (id fields), where id is a symbol
      ; and fields a list of memeber fields (like password, name, etc).
      (let ((id-item (assoc 'id fields)))
        (list (string->symbol (cadr id-item)) fields))))

(define (bindings->tournament bindings)
  (bindings->assoc bindings))

(define (tournament/save tournament)
  (if tournament
    (let ((id (tournament/get 'id tournament)))
      (if id
        (write-to-file
          tournament
          (string-append tournament/path id)
          #:mode 'binary
          #:exists 'truncate/replace)
        #f))
    #f))

(define (tournament/get attr tournament)
  (let ((default-value
          (lambda (attr)
            (cond
              ((symbol=? attr 'members) '())
              (else #f)))))
    (let ((value (assoc attr tournament)))
      (if value (cadr value) (default-value attr)))))

(define (tournament/has? attr tournament)
  (if (assoc attr tournament) #t #f))

(define (tournament/find id)
  (let
    ((data-read
       (lambda (file)
         (if (file-exists? file)
           (file->value file)
           #f))))
    (data-read (string-append tournament/path id))))

(define (tournament/append-member tournament tournament-member)
  (let ((old-members (tournament/get 'members tournament)))
    (if (not (tournament/has? 'members tournament))
      (cons `(members ,(list tournament-member)) tournament)
      (if (assoc (car tournament-member) old-members)
        #f
        (map (lambda (item)
               (if (symbol=? (car item) 'members)
                 (list 'members (cons tournament-member old-members))
                 item))
             tournament)))))

(define (check-pass tournament options)
  (let ((t-pass (assoc 'pass tournament))
        (o-pass (assoc 'pass options)))
    (and t-pass o-pass (bytes=? (cadr t-pass) (cadr o-pass)))))

(define (tournament/start tournament options)
  (if (check-pass tournament options)
    #t
    #f))
