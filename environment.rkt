#lang racket

;; data rep
(struct node [op left right] #:transparent)
(struct decl [variable value scope] #:transparent)

;; enviroment

#; {type Env :
         empty :: Env,
         add :: Var Number Env -> Env,
         defined? :: Var Env -> Any,
         lookup :: Var Env -> Number
         position-of :: Var Env -> N}

(define empty '[])
(define (add x val env) (cons (list x val) env))
(define (defined? x env) (assoc x env))
(define (lookup x env) (second (defined? x env)))

(define (position-of x env)
  (- (length env) (length (member x (map first env)))))

;; interpreter
#; {VE -> Number}
(define (value-of-env ae0)
  #; {VE Env -> Number}
  (define (value-of/acc ae env)
    (match ae
      [(? integer?) ae]
      [(node o a1 a2) (o (value-of/acc a1 env) (value-of/acc a2 env))]
      [(decl x a1 a2) (value-of/acc a2 (add x (value-of/acc a1 env) env))]
      ;;                               in inner scope, the env have later decl'ed var
      ;; env here is an ACCUMULATOR that keep decl'ed var to deeper recrusion.
      ;; Its a pure functional paradigm
      [(? string?)
       (if (defined? ae env)
           (lookup ae env)
           (error 'value-of/acc "undecleared variable ~e" ae))]))
  (value-of/acc ae0 empty))

(define ve-ex1
  (decl "x" (decl "y" 5 {node + "y" "y"})
        (decl "y" 42
              (decl "x" "x"
                    (node + "x" "y")))))

(value-of-env (node + 10 42))

(value-of-env (decl "x" 10
                    (node + "x" 42)))

(value-of-env (decl "x" 10
                    (decl "y" 42
                          (node + "x" "y"))))
