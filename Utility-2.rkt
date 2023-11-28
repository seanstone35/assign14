#lang racket
;resolve variable from scope
;(resolve_env a var_env) -> 1
(define resolve_env
  (lambda
      (varname env)
    (cond
      ((null? env) (error-output "Variable not found."))
      ((eq? #f (resolve_scope varname (car env)))
        (resolve_env varname (cdr env)))
      (else (resolve_scope varname (car env)))
     )
    )
  )


;resolve variable from environment
;(resolve_scope a (car var_env)) -> 1
(define resolve_scope
  (lambda
      (varname scope)
    (cond
      ((null? scope) #f)
      ((eq? varname (car (car scope)))
       (car (cdr (car scope))))
      (else
       (resolve_scope varname (cdr scope)))
      )
    )
  )

;create a new scope with a variable-value pair and push the scope to environment
;(push_var_to_env d 4 env) -> (((d 4)) ...env)
(define push_var_to_env
  (lambda (varName varValue env)
    (cons
     (list (list varName varValue))
     env
     )
    )
  )

;(push_vars_to_env (x y z) (1 2 3)) -> (((x 1) (y 2) (z 3)) env)
(define push_vars_to_env
  (lambda (list_var list_val env)
  (cons (pair_helper list_var list_val) env)
  )
  )

;truncate environment scopes until only global scope left
;not perfect when env is empty or env is not an environment
(define trim_to_global_scope
  (lambda (env)
    (cond
      ((not (pair? env)) (error-output "Illegal environment passed into the trim_to_global_scope argument list"))
      ((eq? 1 (length env)) env)
      (else (trim_to_global_scope (cdr env)))
      )
    )
  )

(define pair_helper
  (lambda (list_var list_val)
    (if (null? list_var)
        '()
        (cons (list (car list_var) (car list_val)) (pair_helper (cdr list_var) (cdr list_val)))
        )
    )
  )
    

(define is_in_list
  (lambda (lst item)
    (cond
      ((null? lst) #f)
      ((eq? (car lst) item) #t)
      (else (is_in_list (cdr lst) item))
      )
    )
  )


(define error-output
  (lambda (output)
    (displayln (string-append "***Error***: " output))
    )
  )

(define display-output
  (lambda (output)
    (displayln (string-append "***Display***: " output))
    )
  )

(define pick_first_non_void_from_list
  (lambda (lst)
    (cond
      ((not (pair? lst)) (error-output "received invalid parameter for a list"))
      ((and (pair? lst) (eq? (length lst) 1) (void? (car lst))) (displayln ""))
      ((void? (car lst)) (pick_first_non_void_from_list (cdr lst)))
      (else (car lst))
     )
    )
  )

(provide (all-defined-out))