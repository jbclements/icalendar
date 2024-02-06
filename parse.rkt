#lang racket

;; following along with RFC 5545 ...

(require rackunit
         irregex)

(define in-port (open-input-file "/tmp/basic.ics"))

;; read all \r\n-terminated lines
(define lines
  (let loop ([lines '()])
    (define try-match
      (regexp-match #px#"^[^\r]*\r\n" in-port))
    (cond [try-match
           (loop (cons (first try-match) lines))]
          [else
           (reverse lines)])))

;; join using "unfolding"
(define (unfold lines)
  (let loop ([lines lines]
             [accum '()])
    (define (advance) (loop (rest lines) (cons (first lines) accum)))
    (cond [(empty? lines) (reverse accum)]
          [(empty? (rest lines)) (advance)]
          ;; could avoid this regexp if it turns out to be slow...
          [(regexp-match #px#"^[ \t]" (second lines))
           (loop (cons (bytes-append (subbytes (first lines) 0
                                               (- (bytes-length (first lines)) 2))
                                     (subbytes (second lines) 1))
                       (rest (rest lines)))
                 accum)]
          [else (advance)])))



(check-equal? (unfold '(#"abc\r\n" #"def\r\n" #"ghi\r\n"))
              '(#"abc\r\n" #"def\r\n" #"ghi\r\n")
              "t1")
(check-equal? (unfold '(#"abc\r\n" #" def\r\n" #"ghi\r\n"))
              '(#"abcdef\r\n" #"ghi\r\n"))
(check-equal? (unfold '(#"xx\r\n" #"abc\r\n" #" def\r\n" #"ghi\r\n"))
              '(#"xx\r\n" #"abcdef\r\n" #"ghi\r\n"))
(check-equal? (unfold '(#"abc\r\n" #" def\r\n" #"ghi\r\n" #"xx\r\n"))
              '(#"abcdef\r\n" #"ghi\r\n" #"xx\r\n"))
(check-equal? (unfold '(#"yy\r\n" #"abc\r\n" #" def\r\n" #"ghi\r\n" #"x x\r\n"))
              '(#"yy\r\n" #"abcdef\r\n" #"ghi\r\n" #"x x\r\n"))
(check-equal? (unfold '(#"abc\r\n" #" def\r\n" #" ghi\r\n"))
              '(#"abcdefghi\r\n" ))
(check-equal? (unfold '(#"xx\r\n" #"abc\r\n" #" def\r\n" #" ghi\r\n"))
              '(#"xx\r\n" #"abcdefghi\r\n" ))
(check-equal? (unfold '(#"xx\r\n" #"abc\r\n" #" def\r\n" #" ghi\r\n" #"yy\r\n"))
              '(#"xx\r\n" #"abcdefghi\r\n" #"yy\r\n"))
(check-equal? (unfold '(#"abc\r\n" #" def\r\n" #" ghi\r\n" #"yy\r\n"))
              '(#"abcdefghi\r\n" #"yy\r\n"))



(count (λ (l) (regexp-match? #px#"^[ \t]" l)) lines)

(define unfolded (unfold lines))

(length lines)
(length unfolded)

(define alpha `(or (/ #\a #\z) (/ #\A #\Z)))
(define digit `(/ #\0 #\9))
;; note that iana-token subsumes x-name, so distinguishing them
;; should happen later.
(define iana-token `(+ (or ,alpha ,digit "-")))
(define name iana-token)
(define value-char `(or ,wsp ))
;; incomplete!
(define contentline `(: (=> name ,name) ":" (=> value (* value-char)) "\r\n"))

(define match
  (irregex-match contentline (bytes->string/utf-8 (first unfolded))))

(irregex-match-substring match 'name)
(irregex-match-substring match 'rest)






;(irregex-search '(/ #\a #\z) "034abcd")

#;(define (parse-line line)
  (match (bytes->string/utf-8 line)
    [(regexp #px"^([-[:alnum:]]*)(.*)\r\n" (list _ name rest)) (list name rest)]))

#;(define parsed (map parse-line unfolded))