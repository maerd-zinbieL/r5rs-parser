; Parser for Scheme. Parses the external representation of a Scheme
; datum:
;
; <datum> --> <simple datum> | <compound datum>
; <simple datum> --> <boolean> | <number> | <character>
;     | <string> | <symbol>
; <symbol> --> <identifier>
; <compound datum> --> <list> | <vector>
; <list> --> (<datum>*) | (<datum>+ . <datum>)
;     | <abbreviation>
; <abbreviation> --> <abbrev prefix> <datum>
; <abbrev prefix> --> ' | ` | , | ,@
; <vector> --> #(<datum>*)
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "lexer.scm")

;;;;;;;;;;;;;;;;;;;

; Determines whether the given token represents a simple datum.
(define (simple-datum? token)
  (let ((type (token-type token)))
    (memq type '(boolean number character string identifier))
  )
)

; Reads and parses a simple datum from standard input, returning the
; Scheme representation of the datum. Returns an eof object if eof is
; encountered. An error is raised if a non-simple datum is
; encountered.
(define (read-simple-datum)
  (let ((first-token (read-token)))
    (cond ((eof-object? first-token) first-token) ; just return eof
          ((not (simple-datum? first-token))
           (error "not a simple datum --read-simple-datum"))
          ; complete this procedure
          (else (parse-simple-datum first-token))
    )
  )
)

(define (parse-simple-datum token)
   (token-data token))

;;;;;;;;;;;;;;;;;;;

; Reads and parses a full compound datum from standard input, which
; can be a proper list, a dotted list, a vector, or an abbreviation.
; Returns the Scheme representation of the datum. For an abbreviation,
; returns the corresponding list representation (e.g. 'hello -> (quote
; hello)). Returns an eof object if eof is encountered. An error is
; raised if a non-compound datum is encountered, or if the compound
; datum is improperly formatted.
(define open-str "(" ) ;;;"for the silly code editor 
(define close-str ")" ) ;;;"for the silly code editor 
(define open-sharp-str "#(" ) ;;;"for the silly code editor 

(define (normal-list-start? token)
  (equal? (token-data token)
          open-str))

(define (list-end? token)
  (if (eof-object? token)
      (error "unexpected end of file")
      (equal? (token-data token)
              close-str)))

(define (abbrev-prefix? token)
   (member (token-data token)
         (list "'" "`" "," ",@")))

(define (vector-start? token)
  (equal? (token-data token)
          open-sharp-str))

(define (datum-start? token)
  (or (simple-datum? token)
      (compound-datum? token)))

(define (compound-datum? token)
  (or (normal-list-start? token)
      (vector-start? token)
      (abbrev-prefix? token)))

(define (parse-normal-list parsed-so-far)
   (let ((token (read-token)))
    ;  (display token)
    ;  (newline)
     (cond ((list-end? token)
            (reverse parsed-so-far))
           ((and (equal? "." (token-data token))
                 (>= (length parsed-so-far) 1))
            (parse-dot-list parsed-so-far))
           ((datum-start? token)
            (parse-normal-list (cons (parse-datum token) parsed-so-far)))
           (else (error "bad list --parse-normal-list")))))

(define (make-dot-list parsed-so-far last-datum)
   (append (reverse (cdr parsed-so-far))
           (cons (car parsed-so-far)
                 last-datum)))

(define (parse-dot-list parsed-so-far)
  (let ((last-datum (parse-datum (read-token))))
    (if (list-end? (read-token))
        (make-dot-list parsed-so-far last-datum)
        (error "expected closing parenthesis"))))

(define (parse-abbreviation prefix)
  (cond ((equal? prefix "'")
         (list 'quote (parse-datum (read-token))))
        ((equal? prefix "`")
         (list 'quasiquote (parse-datum (read-token))))
        ((equal? prefix ",")
         (list 'unquote (parse-datum (read-token))))
        ((equal? prefix ",@")
         (list 'unquote-splicing (parse-datum (read-token))))
        (else (error "bad abbreviation --parse-abbreviation"))))

(define (parse-vector)
  (let ((datum (parse-normal-list
                (list (token-data (read-token))))))
    (if (list? datum)
        (list->vector datum)
        (error "bad vector --parse-vector"))))

(define (parse-compound-datum token)
   (cond ((vector-start? token)
          (parse-vector))
         ((normal-list-start? token)
          (parse-normal-list '()))
         ((abbrev-prefix? token)
          (parse-abbreviation (token-data token)))
         (else (error "bad compound datum --parse-compound-datum"))))
       
(define (read-compound-datum)
  (let ((first-token (read-token)))
    (cond ((eof-object? first-token) first-token) ; just return eof
          ((simple-datum? first-token)
           (error "not a compound datum --read-compound-datum"))
          ; complete this procedure
          ((compound-datum? first-token)
           (parse-compound-datum first-token))
          (else
           (error "not a compound datum --read-compound-datum")))))


;;;;;;;;;;;;;;;;;;;

; Reads a complete datum from standard input. If eof is encountered,
; returns an eof object. Raises an error if an improper token or datum
; is encountered.
(define (read-datum)
  (let ((first-token (read-token)))
    (if (eof-object? first-token)
        first-token ; just return eof
        (read-datum-helper first-token)
    )
  )
)

; Reads a datum from standard input, where the datum's first (and
; possibly only) token is first-token. Raises an error if the datum is
; not properly formatted.
(define (parse-datum token)
  (cond ((simple-datum? token)
         (parse-simple-datum token))
        ((compound-datum? token)
         (parse-compound-datum token))
        (else
         (error "bad datum --parse-datum"))))

(define (read-datum-helper first-token)
  ; '() ; replace with your code
  (parse-datum first-token))
 
