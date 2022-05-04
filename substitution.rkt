#lang racket

(module+ test
  (require rackunit))

(struct node [op left right] #:transparent)
(struct decl [variable value scope] #:transparent)

;; subst ->

(define (subst val x ae)
  (match ae
    [(? integer?) ae]
    [(node o a1 a2) (node o (subst val x a1) (subst val x a2))]
    [(decl y a1 scope)
     (if (equal? x y) (decl y (subst val x a1) scope) (decl y (subst val x a1) (subst val x scope)))]
    [(? string?) (if (equal? ae x) val ae)]))

(module+ test
  (check-equal? (subst 5 "y" (decl "x" 10 (node + "x" "y"))) (decl "x" 10 (node + "x" 5))))

(define (value-of-subst ae)
  (match ae
    [(? integer?) ae]
    [(node o a1 a2) (o (value-of-subst a1) (value-of-subst a2))]
    [(decl x a1 a2) (value-of-subst (subst (value-of-subst a1) x a2))]))

(module+ test
  (check-equal? 15 (value-of-subst (decl "x" 10 (decl "y" 5 (node + "x" "y"))))))
