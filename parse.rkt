#lang typed/racket

;; following along with RFC 5545 ...

(require typed/rackunit
         #;irregex
         )

(define in-port (open-input-file "/tmp/basic.ics"))

;; read all \r\n-terminated lines
(define lines : (Listof Bytes)
  (let loop  ([lines : (Listof Bytes) '()])
    (define try-match
      (regexp-match #px#"^[^\r]*\r\n" in-port))
    (cond [try-match
           (loop (cons (first try-match) lines))]
          [else
           (reverse lines)])))

;; join using "unfolding"
(define (unfold [lines : (Listof Bytes)]) : (Listof Bytes)
  (let loop ([lines lines]
             [accum : (Listof Bytes) '()])
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



(count (λ ([l : Bytes]) (regexp-match? #px#"^[ \t]" l)) lines)

(define unfolded (unfold lines))

(length lines)
(length unfolded)
#;(
(define alpha `(or (/ #\a #\z) (/ #\A #\Z)))
(define digit `(/ #\0 #\9))
;; note that iana-token subsumes x-name, so distinguishing them
;; should happen later.
(define iana-token `(+ (or ,alpha ,digit "-")))
(define name iana-token)
(define wsp '(" \t"))
;; the range (or (/ #\u21 #\ud7ff) (/ #\uE000 #\U10FFFF))
;; appears to me to correspond
;; to the specification "%x21-7E / NON-US-ASCII"
;; where NON-US-ASCII is defined as
;; UTF8-2 / UTF8-3 / UTF8-4
;; I have investigated this as much as I choose to at this moment....
;; I'm also slightly concerned about the possibility that irregex
;; is actually constructing a regex that includes each character
;; by name...
(define value-char `(or ,wsp (/ #\u21 #\ud7ff) (/ #\uE000 #\U10FFFF)))

;; param-name is the same as name...

;SAFE-CHAR     = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E
(define safe-char `(or ,wsp #\u21 (/ #\u23 #\u2b) (/ #\u2d #\u39) (/ #\u3c #\u7e)))
(define paramtext `(* ,safe-char))
;QSAFE-CHAR    = WSP / %x21 / %x23-7E / NON-US-ASCII
; Any character except CONTROL and DQUOTE
(define qsafe-char `(or ,wsp #\u21 (/ #\u23 #\u7e #\ud7ff) (/ #\uE000 #\U10FFFF)))
;quoted-string = DQUOTE *QSAFE-CHAR DQUOTE
(define quoted-string `("\"" (* ,qsafe-char) "\""))
(define param-value `(or ,paramtext ,quoted-string))
(define param `(: ,name "=" ,param-value (* (: "," ,param-value))))

;; incomplete!
(define contentline
  `(: (=> name ,name)
      (* ";" ,param)
      ":" (=> value (* ,value-char)) "\r\n"))
)
;; sadly, irregex seems way too slow.

;okay, doing it as a big fat string construction, ugh...


#;(define parsed
  (time
  (for/list ([l (in-list (take unfolded 2000))])
    (define match
      (irregex-match contentline (bytes->string/utf-8 l)))

    (when (not match)
      (error 'parsing "couldn't parse this one: ~e" l))
    (irregex-match-substring match 'name)
    (irregex-match-substring match 'value))))

(define iana-token "[-[:alnum:]]*")
(define value-char "[ \t\u21-\ud7ff\ue000-\U10ffff]")
;SAFE-CHAR     = WSP / %x21 / %x23-2B / %x2D-39 / %x3C-7E
(define safe-char "[- \t\u21\u23-\u2b\u2e-\u39\u3c-\u7e]")
(define paramtext (~a safe-char "*"))
;QSAFE-CHAR    = WSP / %x21 / %x23-7E / NON-US-ASCII
; Any character except CONTROL and DQUOTE
(define qsafe-char "[ \t\u21\u23-\ud7ff\ue000-\U10fff]")
;quoted-string = DQUOTE *QSAFE-CHAR DQUOTE
(define quoted-string (~a "\""qsafe-char"*\""))
(define param-value (~a "("paramtext"|"quoted-string")"))

(define param (~a "(;("iana-token")=("param-value")((,"param-value")*))"))

(define contentline (~a "^("iana-token")" ;;#1
                        ;; ouch... the limitations of pregexps (no nested
                        ;; match sequences) mean that we need to parse this
                        ;; in multiple steps, sigh.
                        "("param"*)" ;; #2, #3
                        ":("value-char"*)\r\n$" ;; #4
                        ))
  
(regexp-match (pregexp contentline)
              "BEGIN:VCALENDAR\r\n")

(define the-regexp (pregexp contentline))

(define param-regexp (pregexp (~a "^" param "("param"*)$")))

(define othervals-regexp (pregexp (~a "^,"param-value"((,"param-value")*)$")))

(struct Parsed-Line ([name : String]
                     [params : (Listof (List String (Listof String)))]
                     [value : String]) #:transparent)



;; parse a line
(define (parse-line [l : Bytes]) : Parsed-Line
  (define m
    (regexp-match the-regexp (bytes->string/utf-8 l)))
  (match m
    [#f (error 'parsing "couldn't parse this one: ~e" l)]
    [(list _ name params _ _ _ _ _ _ _ value)
     ;; assert should succeed by def'n of regexp
     (Parsed-Line (assert name string?)
                  (parse-params (assert params string?))
                  (assert value string?))]))

;; parse the remaining values
(define (parse-param-othervals [othervals : String]) : (Listof String)
  (match othervals
    ["" '()]
    [other
     (match (regexp-match othervals-regexp othervals)
       [(list _ firstval remainder _ _)
        (cons (assert firstval string?)
              (parse-param-othervals (assert remainder string?)))])]))

;; parse the param list
(define (parse-params [params : String])
  : (Listof (List String (Listof String)))
  (match params
    ["" '()]
    [other
     (match (regexp-match param-regexp params)
       [(list _ _ p1-name p1-val1 _ p1-othervals _ _ rest _ _ _ _ _ _ _)
        (cons (list (assert p1-name string?)
                    (cons (assert p1-val1 string?)
                          (parse-param-othervals
                           (assert p1-othervals string?))))
              (parse-params (assert rest string?)))]
       [#f (error 'parse-params "no match for params: ~e" params)])]))



(parse-line #"ABC;A=3,\"4\",5,\"6\";B=4:euaouth\r\n")
(parse-line #"ABC;A=3,\"4\",5;B=4;c=5,6:euaouth\r\n")

(define parsed : (Listof Parsed-Line)
  (time
   (map parse-line unfolded)))


#;(define parsed2)

(regexp-match #rx"[\u21-\ud7ff]" "@")




#;(check-equal? (irregex-match-data?
               (irregex-match
                `(* ,value-char)
                "aeu23<234<@u33onth.  ah\t~\"" ))
              #t)


;(irregex-search '(/ #\a #\z) "034abcd")

#;(define (parse-line line)
  (match (bytes->string/utf-8 line)
    [(regexp #px"^([-[:alnum:]]*)(.*)\r\n" (list _ name rest)) (list name rest)]))

#;(define parsed (map parse-line unfolded))