#lang racket/base

(require "main.rkt" racket/set)
(provide go)
(define (go v path src cust)
  (cond
    [(exn? v) #f]
    [else
     (for/list ([s (in-set (walk v))])
       (symbol->string s))]))