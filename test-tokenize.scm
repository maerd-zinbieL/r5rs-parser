; Tokenizes all of standard input and writes out the tokens to
; standard input, one per line.
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "lexer.scm")

(define (write-all lst)
  (if (not (null? lst))
      (begin (write (car lst))
             (newline)
             (write-all (cdr lst))
      )
  )
)

(write-all (tokenize))
