#lang racket

(provide parse)

(define (parse-value string)
  (or (string->number string)
      string))

(define (parse-props props)
  (make-hash
   (map (lambda (string)
          (match (string-split string "=")
            [(list key value) (cons key (parse-value value))]
            [else (error "Syntax error: can't parse property: " string)]))
        props)))

(define (parse-clause clause)
  (match clause
    [(list keyword id props ...) (list keyword id (parse-props props))]
    [null null]
    [else (error "Syntax error: can't parse clause" clause)]))

(define (token+ token char)
  (cons char token))

(define (clause+ clause raw-token)
  (let ([token (list->string (reverse raw-token))])
    (if (non-empty-string? token)
        (cons token clause)
        clause)))

(define (result+ result raw-clause)
  (let ([clause (parse-clause (reverse raw-clause))])
    (if (null? clause)
        result
        (cons clause result))))

(define (eq . xs)
  (lambda (y) (set-member? xs y)))

(define (parse* in end-of-token? token clause result)
  (let ([char (read-char in)])
    (cond
     [(eof-object? char)    (reverse result)]
     [((eq #\newline) char) (parse* in end-of-token? '() '() (result+ result (clause+ clause token)))]
     [(end-of-token? char)  (parse* in (eq #\space) '() (clause+ clause token) result)]
     [((eq #\") char)       (parse* in (eq #\") token clause result)]
     [else                  (parse* in end-of-token? (token+ token char) clause result)])))

(define (parse in)
  (parse* in (eq #\space) null null null))
