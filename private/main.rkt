#lang racket/base
(require racket/set syntax/kerncase)
(provide walk)

(define (walk* stxs)
  (let loop ([ls (syntax->list stxs)])
    (if (null? ls)
        (seteq)
        (set-union (walk (car ls)) (loop (cdr ls))))))

(define (visible? id)
  (for/and ([scope (in-list
                    (hash-ref (syntax-debug-info id)
                              'context (λ () '())))])
    (not (eq? 'macro (vector-ref scope 1)))))

(define (visible stx)
  (syntax-case stx ()
    [(a . b)
     (set-union (visible #'a) (visible #'b))]
    [x
     (identifier? #'x)
     (if (visible? #'x)
         (seteq (syntax-e #'x))
         (seteq))]
    [_ (seteq)]))

(define (walk stx)
  (define ret
    (kernel-syntax-case stx #f
      [(#%expression ?expr) (walk #'?expr)]
      [(module _ _ (#%plain-module-begin ?module-level-form ...))
       (walk* #'(?module-level-form ...))]
      [(begin ?expr ...)
       (walk* #'(?expr ...))]
      [(begin0 ?expr ...)
       (walk* #'(?expr ...))]
      [(begin-for-syntax ?expr ...)
       (walk* #'(?expr ...))]
      [(define-values (?id ...) ?expr)
       (set-union (visible #'(?id ...)) (walk #'?expr))]
      [(define-syntaxes (?id ...) ?expr)
       (set-union (visible #'(?id ...)) (walk #'?expr))]
      [(#%plain-lambda ?formals ?expr ...)
       (set-union (visible #'?formals) (walk* #'(?expr ...)))]
      [(case-lambda (?formals ?expr ...) ...)
       (let loop ([formals (syntax->list #'(?formals ...))]
                  [set (walk* #'(?expr ... ...))])
         (cond
           [(null? formals) set]
           [else (loop (cdr formals)
                       (set-union (visible (car formals))
                                  set))]))]
      [(if ?expr ...)
       (walk* #'(?expr ...))]
      [(let-values ([(?id ...) ?expr] ...)
         ?body ...)
       (set-union (visible #'(?id ... ...))
                  (walk* #'(?expr ... ?body ...)))]
      [(letrec-values ([(?id ...) ?expr] ...)
         ?body ...)
       (set-union (visible #'(?id ... ...))
                  (walk* #'(?expr ... ?body ...)))]
      [(set! ?id ?expr)
       (walk #'?expr)]
      [(with-continuation-mark ?expr ...)
       (walk* #'(?expr ...))]
      [(#%plain-app ?expr ...)
       (walk* #'(?expr ...))]
      [_ (seteq)]))
  
  (cond
    [(syntax-property stx 'disappeared-binding)
     =>
     (λ (ls)
       (set-union (visible (datum->syntax #'k ls)) ret))]
    [else ret]))