#lang racket

(provide parse)

(define (parse in)
  (let ([line (read-line in)])
    (cond
      [(eof-object? line) eof]
      [(non-empty-string? line) (parse-line line)]
      [else (parse in)])))

(define (parse-line string)
  (match (string-split string)
    [(list keyword id props ...) (list keyword id (parse-props props))]
    [else (raise "Syntax error: can't parse line")]))

(define (parse-props props)
  (make-hash
   (map (lambda (string)
          (match (string-split string "=")
            [(list key value) (cons key value)]
            [else (raise "Syntax error: can't parse property")]))
        props)))
