; Lexer for Scheme. The following is the lexical specification that it
; handles:
;
; <token> --> <identifier> | <boolean> | <number>
;     | <character> | <string> | ( | ) | #( | ' | ` | , | ,@ | .
; <delimiter> --> <whitespace> | ( | ) | " | ;
; <whitespace> --> <space or newline>
; <comment> --> ; <all subsequent characters up to a line break>
; <atmosphere> --> <whitespace> | <comment>
; <intertoken space> --> <atmosphere>*
;
; <identifier> --> <initial> <subsequent>*
;     | <peculiar identifier>
; <initial> --> <letter> | <special initial>
; <letter> --> [a-z]
;
; <special initial> --> ! | $ | % | & | * | / | : | < | =
;     | > | ? | ^ | _ | ~
; <subsequent> --> <initial> | <digit> | <special subsequent>
; <digit> --> [0-9]
; <special subsequent> --> + | - | . | @
; <peculiar identifier> --> + | - | ...
;
; <boolean> --> #t | #f
; <character> --> #\ <any character> | #\ <character name>
; <character name> --> space | newline
;
; <string> --> " <string element>* "
; <string element> --> <any character other than " or \>
;     | \" | \\
;
; <number> --> <integer> | <decimal>
; <integer> --> <sign> <digit>+
; <decimal> --> <sign> <digit>+ . <digit>*
;     | <sign> . <digit>+
;
; <sign> --> <empty> | + | -
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "distribution.scm")

;;;;;;;;;;;;;;;;;;;

; Read a string token.
(define (read-string)
  (if (read-start #\" "not a string") ; string must start with "
      (read-string-tail '()) ; call helper function below
  )
)

; Read the rest of a string literal.
(define (read-string-tail read-so-far)
  (let ((next-char (get-non-eof-char))) ; read a single char
    (cond ((char=? next-char #\") ; end of string
           ; return a string token
           (token-make 'string (list->string (reverse read-so-far))))
          ((char=? next-char #\\) ; start of escape sequence
           ; read the rest of the escape sequence and recurse
           (read-string-tail (cons (read-escaped) read-so-far)))
          ; complete this procedure
          (else (read-string-tail (cons next-char read-so-far)))
    )
  )
)

; Read the rest of an escape sequence.
(define (read-escaped)
  (let ((escaped-char (get-non-eof-char)))
    (if (or (char=? escaped-char #\") (char=? escaped-char #\\))
        escaped-char
        (error "unrecognized escape sequence")
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a boolean token.
(define (read-boolean)
  (if (read-start #\# "not a boolean") ; boolean starts with #
      (read-boolean-tail)
  )
)

; Read the rest of a boolean literal.
(define (read-boolean-tail)
  ; '() ; replace with your code
  (let ((next-char (get-non-eof-char)))
    (cond ((and (char=? next-char #\t)
                (delimiter? (peek-char)))
           (token-make 'boolean #t))
          ((and (char=? next-char #\f)
                (delimiter? (peek-char)))
           (token-make 'boolean #f))
          (else (error "bad boolean constant"))))
)

;;;;;;;;;;;;;;;;;;;

; Read a character token.
(define (read-character)
  (if (and (read-start #\# "not a character")  ; character must start
           (read-start #\\ "not a character")) ; with #\
      (read-character-tail)
  )
)

; Read the rest of a character literal.
(define (read-character-tail)
  ; '() ; replace with your code
  (let ((next-char (get-non-eof-char)))
    (cond ((delimiter? (peek-char))
           (token-make 'character next-char))
          ((and (char=? next-char #\s)
                (read-start #\p "bad character constant")
                (read-start #\a "bad character constant")
                (read-start #\c "bad character constant")
                (read-start #\e "bad character constant")
                (delimiter? (peek-char)))
           (token-make 'character #\space))
          ((and (char=? next-char #\n)
                (read-start #\e "bad character constant")
                (read-start #\w "bad character constant")
                (read-start #\l "bad character constant")
                (read-start #\i "bad character constant")
                (read-start #\n "bad character constant")
                (read-start #\e "bad character constant")
                (delimiter? (peek-char)))
           (token-make 'character #\newline))
          (else (error "bad character constant")))))
          


;;;;;;;;;;;;;;;;;;;

; Determine if the given character is a sign character.
(define (sign? char)
  (or (char=? char #\+) (char=? char #\-))
)

; Determine if the given character is a digit.
(define (digit? char)
  (and (char>=? char #\0) (char<=? char #\9))
)

(define (char->digit char)
  (- (char->integer char) 48))

; Read a number token.
(define (read-number)
(let ((char (peek-char)))
   (if (or (sign? char)
           (digit? char)
           (char=? char #\.))
        (read-number-tail)
        (error "not a number"))))

(define (read-number-tail)
  ; '() ; replace with your code
  (let ((start-char (get-non-eof-char)))
    (cond ((sign? start-char)
           (read-number-integral start-char 0))
          ((digit? start-char)
           (read-number-integral #\+ (char->digit start-char)))
          ((char=? start-char #\.)
           (read-number-fractional #\+ 0.1 0))
          (else (error "not a number")))))

(define (read-number-fractional sign base number)
  (let ((next-char (get-non-eof-char)))
    (cond ((digit? next-char)
           (read-number-real sign
            (/ base 10)
            (+ number  (* (char->digit next-char)  base))))
          (else (error "bad number")))))

(define (read-number-integral sign number)
  (let ((next-char (peek-char)))
    (cond ((digit? next-char)
           (read-number-integral sign
            (+ (* number 10) (char->digit (get-non-eof-char)))))
          ((char=? next-char #\.)
           (get-non-eof-char)
           (read-number-real sign 0.1 number))
          ((delimiter? next-char)
           (number-token-make sign number))
          (else (error "bad number")))))

(define (read-number-real sign base number)
  (let ((next-char (peek-char)))
    (cond ((digit? next-char)
           (read-number-real sign (/ base 10)
            (+ number (* (char->digit (get-non-eof-char)) base))))
          ((delimiter? next-char)
           (number-token-make sign number))
          (else (error "bad number")))))

(define (number-token-make sign number)
  (let ((represent (if (integer? number)
                       number
                       (exact->inexact number))))
    (if (char=? sign #\-)
        (token-make 'number (* -1 represent))
        (token-make 'number represent))))

;;;;;;;;;;;;;;;;;;;


; Read an identifier token.
(define (read-identifier)
  '() ; replace with your code
  (let ((next-char (char-downcase(get-non-eof-char))))
    (cond ((or (letter? next-char)
               (special-initial? next-char))
           (read-normal-identifier `(,next-char)))
          ((sign? next-char)
           (if (delimiter? (peek-char))
               (read-identifier-sign next-char)
               (error "not an identifier")))
          ((char=? next-char #\.)
           (read-identifier-dots3 1))
          (else (error "not an identifier")))))

(define (read-identifier-sign next-char)
    (token-make 'identifier (string->symbol (string next-char))))

(define (read-identifier-dots3 count)
  (let ((next-char (char-downcase(peek-char))))
    (cond ((and (delimiter? next-char)
                (= count 3))
           (token-make 'identifier '...))
          ((and (<= count 3)
                (char=? next-char #\.))
           (get-non-eof-char)
           (read-identifier-dots3 (+ count 1)))
          (else (error "bad indentifier")))))

(define (read-normal-identifier read-so-far)
  (let ((next-char (char-downcase(peek-char))))
    (cond ((delimiter? next-char)
           (token-make 'identifier (string->symbol
                                    (list->string
                                     (reverse read-so-far)))))
          ((or (special-subsequent? next-char)
               (digit? next-char)
               (special-initial? next-char)
               (letter? next-char))
           (read-normal-identifier
             (cons (get-non-eof-char) read-so-far)))
          (else (error "bad identifier")))))
          
; (define (initial? char)
;   (or (letter? char) (special-initial char)))

(define (letter? char)
  (or (and (char>=? char #\A)
           (char<=? char #\Z))
      (and (char>=? char #\a)
           (char<=? char #\z))))

(define (special-initial? char)
  (let ((specials '(#\!
                    #\$
                    #\%
                    #\$
                    #\*
                    #\/
                    #\:
                    #\<
                    #\=
                    #\>
                    #\?
                    #\^
                    #\_
                    #\~)))
    (memq char specials)))

(define (subsequent? char)
  (or (initial? char)
      (digit? char)
      (special-subsequent? char)))

(define (special-subsequent? char)
  (or (char=? char #\+)
      (char=? char #\-)
      (char=? char #\.)
      (char=? char #\@)))

      
;;;;;;;;;;;;;;;;;;;


; Read a punctuator token (i.e. one of ( ) #( . ' ` , ,@ ).
(define open #\( )
(define close #\) )
(define open-sharp-str "#(" ) ;;;" for the silly code editor

(define (read-punctuator-open-sharp)
    (get-non-eof-char)
    (token-make 'punctuator open-sharp-str))

(define (read-punctuator-comma)
  (if (char=? #\@ (peek-char))
      (begin
        (get-non-eof-char)
        (token-make 'punctuator ",@"))
      (begin
        (token-make 'punctuator ","))))

(define (read-punctuator)
  ; '() ; replace with your code
  (let ((next-char (get-non-eof-char)))
    (cond ((or (char=? next-char open)
               (char=? next-char close)
               (char=? next-char #\.)
               (char=? next-char #\')
               (char=? next-char #\`))
           (token-make 'punctuator (string next-char)))
          ((and (char=? next-char #\#)
                (char=? open (peek-char)))
           (read-punctuator-open-sharp))
          ((char=? next-char #\,)
           (read-punctuator-comma))
          (else (error "bad punctuator")))))

    
;;;;;;;;;;;;;;;;;;;

; Read a comment. Discards the data and returns an unspecified value.
(define (read-comment)
  (if (read-start #\; "not a comment")
      (read-comment-tail)
  )
)

; Read the rest of a comment.
(define (read-comment-tail)
  (clear-line)
)
;;;;;;;;;;;;;;;;;;;

; Read a token, which can be a boolean, character, string, identifier,
; number, or punctuator. Discards whitespace and comments.
(define (read-token-sharp)
  ; maybe punctuator character boolean
   (read-char)
   (let ((next-char (peek-char)))
     (cond ((or (char=? next-char #\t)
                (char=? next-char #\f))
            (read-boolean-tail))
           ((char=? next-char open)
            (read-punctuator-open-sharp))
           ((char=? next-char #\\)
            (read-char)
            (read-character-tail))
           (else (error "bad token --read-token-sharp")))))

(define (read-token-dot)
  ;  maybe identifier number punctuator
    (read-char)
    (let ((next-char (peek-char)))
      (cond ((digit? next-char)
             (read-number-fractional #\+ 0.1 0))
            ((char=? next-char #\.)
             (read-identifier-dots3 1))
            ((delimiter? next-char)
             (token-make 'punctuator "."))
            (else "bad token --read-token-dot"))))

(define (read-token-sign)
;maybe identifier number
  (read-char)
  (let ((next-char (peek-char)))
    (cond ((delimiter? next-char)
           (read-identifier-sign next-char))
          ((digit? next-char)
           (read-number-integral next-char 0))
          (else (error "bad token --read-token-sign")))))

(define (string-start? char)
   (char=? char #\"))

(define (exact-number-start? char)
   (digit? char))

(define (exact-identifier-start? char)
  (or (letter? char)
      (special-initial? char)))

(define (exact-punctuator-start? char)
  (memq char `(,open ,close #\' #\` #\,)))

(define (read-token)
  (let ((next-char (peek-char)))
    ; (display next-char)
    (cond ((eof-object? next-char) ; eof
           (read-char)) ; just return eof
          ((whitespace? next-char) ; whitespace
           (read-char) ; discard it
           (read-token)) ; read another token
          ((char=? next-char #\;) ; comment
           (read-comment) ; discard it
           (read-token)) ; read another token
          ; complete this procedure
          
          ((char=? next-char #\#)
           (read-token-sharp))
          
          ((char=? next-char #\.)
           (read-token-dot))
          
          ((sign? next-char)
           (read-token-sign))

          ((string-start? next-char)
           (read-char)
           (read-string-tail '()))
    
          ((exact-number-start? next-char)
           (let ((digit (read-char)))
             (read-number-integral #\+ (char->digit digit))))
          ((exact-identifier-start? next-char)
           (read-normal-identifier `(,(char-downcase (read-char)))))
          ((exact-punctuator-start? next-char)
           (read-punctuator))
          (else
           (error "bad token --read-token"))
    )
  )
)

; Lexer interface: reads all data from standard input and produces a
; list of tokens if no error arises. Aborts if there is an error.
(define (tokenize)
  (tokenize-tail '())
  )


(define (tokenize-tail read-so-far)
  (let ((next-token (read-token)))
    (if (eof-object? next-token)
        (reverse read-so-far)
        (tokenize-tail (cons next-token read-so-far))
    )
  )
)
