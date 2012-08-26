#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require file/md5)
(provide
  bindings->member
  bindings->tourney
  tourney/save
  tourney/get
  tourney/has?
  tourney/find
  tourney/append-member)

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
