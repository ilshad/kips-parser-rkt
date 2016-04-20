#lang s-exp syntax/module-reader

#:read read-kips
#:read-syntax read-kips-syntax

(require "../parser.rkt")

(define (read-kips in)
  (syntax->datum (read-kips-syntax in)))

(define (read-kips-syntax in)
  (parse in))
