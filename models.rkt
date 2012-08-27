#lang racket
; Copyright (C) 2012 Rafael Cunha de Almeida <rafael@kontesti.me>
;
; Copying and distribution of this file, with or without modification,
; are permitted in any medium without royalty provided the copyright
; notice and this notice are preserved.  This file is offered as-is,
; without any warranty.

(require "assoc-lib.rkt")
(require file/md5)
(provide
  bindings->assoc
  bindings->player
  bindings->tournament
  tournament/save
  tournament/get
  tournament/has?
  tournament/find
  tournament/append-player
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

(define (bindings->player bindings)
    (let ((fields (bindings->assoc bindings)))
      ; create a player entry with the format (id fields), where id is a symbol
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
  (cond
    ((symbol=? attr 'players)
     (or (assoc-value 'players tournament) '()))
    ((symbol=? attr 'player-nick-list)
     (map
       (lambda (x) (symbol->string (car x)))
       (tournament/get 'players tournament)))
    ((symbol=? attr 'groups)
     (or (assoc-value 'groups tournament) '()))
    ((symbol=? attr 'group-stats)
     ; still missing actual stats
     (map
       (lambda (group)
         (list
           (car group)
           ((map
              (lambda (player)
                `((name ,(symbol->string (car player)))))
              (cdr group)))))
       (tournament/get 'groups tournament)))
    (else (assoc-value attr tournament))))

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

(define (tournament/append-player tournament tournament-player)
  (let ((old-players (tournament/get 'players tournament)))
    (if (not (tournament/has? 'players tournament))
      (cons `(players ,(list tournament-player)) tournament)
      (if (assoc (car tournament-player) old-players)
        #f
        (map (lambda (item)
               (if (symbol=? (car item) 'players)
                 (list 'players (cons tournament-player old-players))
                 item))
             tournament)))))

; check if password matches the one passed as options
(define (check-pass tournament options)
  (let ((t-pass (assoc 'pass tournament))
        (o-pass (assoc 'pass options)))
    (and t-pass o-pass (bytes=? (cadr t-pass) (cadr o-pass)))))

(define (assoc-get-number name association)
  (let ((item (assoc name association)))
    (if item
      (let ((number (string->number (cadr item))))
        number)
      #f)))

; check if number of groups and group-winners make sense
(define (check-sizes tournament options)
  (let ((num-groups (assoc-get-number 'num-groups options))
        (num-group-winners (assoc-get-number 'num-group-winners options))
        (players (length (tournament/get 'players tournament))))
    (and
      num-groups
      num-group-winners
      (>= players num-groups)
      (>= players num-group-winners))))

(define (make-groups players num-groups)
  (let ((enumerate (lambda (list-of-groups)
                     (let ((i 0))
                       (map
                         (lambda (group) (set! i (+ i 1)) (cons i (list group)))
                         list-of-groups)))))
    (enumerate
      (foldr
        (lambda (player groups)
          (if (empty? groups)
            (cons (list (car player)) groups)
            (let ((current-group (car groups)))
              (if (< (length current-group) num-groups)
                (cons (cons (car player) current-group) (cdr groups))
                (cons (list (car player)) groups)))))
        '()
        players))))

(define (cartesian-product ls)
  (apply append
         (map (lambda (x)
                (map (lambda (y)
                       (list x y)) ls)) ls)))

;(define (make-matches groups)
;  (let
;    ((make-matches-from-group
;       (lambda (group)
;         (letrec
;           ((play-other? (lambda (match) (not (symbol=? (car match) (cadr match)))))
;            (same-match? (lambda (match1 match2)
;                          (or
;                            (and (symbol=? (car match1) (car match2)) (symbol=? (cadr match1) (cadr match2)))
;                            (and (symbol=? (cadr match1) (car match2)) (symbol=? (car match1) (cadr match2))))))
;            (all-combinations (cartesian-product (cdr group)))
;            (add-key (lambda (match) `(vs (,(car match) . #f) (,(cadr match) . #f)))))
;           (map
;             add-key
;             (remove-duplicates (filter play-other? all-combinations) same-match?))))))
;    (foldr append '() (map make-matches-from-group groups))))

(define (tournament/start tournament options)
  (if (and
        (check-pass tournament options)
        (check-sizes tournament options)
        (not (tournament/get 'started tournament)))
    (let ((num-groups (assoc-get-number 'num-groups options))
          (num-group-winners (assoc-get-number 'num-group-winners options))
          (players (tournament/get 'players tournament)))
      (let ((players/group (/ (length players) num-groups)))
        (let ((groups (make-groups (shuffle players) players/group)))
          (let ((matches '()))
            (tournament/save
              (append
                (list
                  '(started #t)
                  `(num-group-winners ,num-group-winners)
                  `(groups ,groups)
                  `(matches ,matches))
              tournament))))))
    #f))
