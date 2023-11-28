#lang racket
(require "Utility.rkt")

;(var-exp a)
;(processor (var-exp a)) -> (resolve a variable_env) -> 1
(define process_var_exp
  (lambda
      (parsedCode env)
     (resolve_env (cadr parsedCode) env)
    )
  )

;(num-exp 1) -> 1
(define process_num_exp
  (lambda
      (parsedCode env)
    (cadr parsedCode)
    )
  )
    

;(processor (app-exp (func-exp (list-exp (var-exp x) (var-exp y)) (var-exp x))
;(list-exp (var-exp a) (num-exp 5)))
;(list-exp (var-exp x) (var-exp y)) -> (x y)
;(list-exp (var-exp a) (num-exp 5)) -> (process (var-exp a)) (process (num-exp 5)
;function func(x){...}; func(1)
(define process_app_exp
  (lambda
      (parsedCode env)
    (let*
        (
         ;structure of env ((.pairs.)(.pairs.)...(.pairs..)(global-variable-scope))
         (global_env (trim_to_global_scope env));this is the environment only has the global scope;
;local_env only has the variable-value paris of the argument list and the global variable scope
         (local_env
          (push_vars_to_env
           (map (lambda (arg) (cadr arg)) (cdr (car (cadr (cadr parsedCode)))))
           (map (lambda (val-exp) (processor val-exp env))
                (cdr (caddr parsedCode)))
           global_env)
          )
         )
      (processor (caddr (cadr parsedCode)) local_env)
      )
    )
  )

;(bool-exp == (var-exp a) (num-exp 1))
(define process_bool_exp
  (lambda
   (parsedCode env)
   (cond
     ((eq? '> (cadr parsedCode))
      (> (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '< (cadr parsedCode))
      (< (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '>= (cadr parsedCode))
      (>= (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '<= (cadr parsedCode))
      (<= (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '== (cadr parsedCode))
      (eq? (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '&& (cadr parsedCode))
      (and (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '|| (cadr parsedCode))
      (or (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '! (cadr parsedCode))
      (not (processor (caddr parsedCode) env)))
     ((eq? '!= (cadr parsedCode))
      (not (eq? (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env))))
     (else (error-output "Illegal boolean expression"))
      )
   )
  )

(define process_ask_exp
  (lambda
   (parsedCode env)
   (if
    (processor (cadr parsedCode) env)
    (processor (caddr parsedCode) env)
    (processor (cadddr parsedCode) env))
   )
  )

;process math ('+ '- '* '/ '// '%)
(define process_math_exp
  (lambda
   (parsedCode env)
   (cond
     ((eq? '+ (cadr parsedCode))
      (+ (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '- (cadr parsedCode))
      (- (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '* (cadr parsedCode))
      (* (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '/ (cadr parsedCode));integer division 5/2 = 2
      (quotient (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '// (cadr parsedCode));float division, give you mix number 5/2 = 2 and 1/2
      (/ (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     ((eq? '% (cadr parsedCode))
      (modulo (processor (caddr parsedCode) env) (processor (cadddr parsedCode) env)))
     (else (error-output "Illegal math expression"))
      )
   )
  )

;(let-exp
;(list-exp ((var-exp d) (num-exp 10)) ((var-exp f) (num-exp 20)))
;(math-exp + (var-exp d) (math-exp + (var-exp f) (var-exp x))
(define process_let_exp
  (lambda (parsedCode env)
    (let*
        ((varname_value_list;varname_value_list = ((d 10) (e 20))
          (map (lambda (pair)
                 (list (cadr (car pair)) (processor (cadr pair) env)))
               (cdr (cadr parsedCode))))
         ;((d 10) (e 20)) + (((x 5)) ((a 1) (b 2) (c 3)))
         ;(((d 10) (e 20) (x 5)) ((a 1) (b 2) (c 3)))
         (let_local_env (cons (append varname_value_list (car env)) (cdr env))))
      (processor (caddr parsedCode) let_local_env)
     )
    )
  )

;(assign-exp ((var-exp x) (num-exp 10)))
(define process_assign_exp
  (lambda (parsedCode env)
     (let*
        ((varname (cadr (car (car (cdr parsedCode)))))
         (value (processor (cadr (car (cdr parsedCode))) env))
         (is_varname_in_env (is_in_list (combine (extract_varname_from_env env)) varname)))
    (if is_varname_in_env
        (update_varaible_in_env varname value env)
        (cons (cons (list varname value) (car env)) (cdr env)))
     )
    )
  )

;parsedCode =(when-exp (bool-exp < a 5)(block-exp (output-exp (var-exp a)) (let-exp ...)
(define process_when_exp
  (lambda (parsedCode env)
    (let
        ((condition (processor (cadr parsedCode) env))
         (true_body_exp (append (cdr (caddr parsedCode)) (list parsedCode))))
      (erase_void (if condition
          ;(list-exp (assign-exp ((var-exp a) (math-exp + (var-exp a) (num-exp 1)))) (output-exp (var-exp a))))
          ;append the parsedCode to the parsedCode
          ;execute every code
          (process_when_exp_body true_body_exp env)
          ;otherwise stop
          (println "when-loop stop here")
       ))
      )
  )
  )

(define process_when_exp_body
  (lambda (body env)
    (cond
      ((null? body) '())
      ((eq? 'assign-exp (car (car body)))
       (process_when_exp_body (cdr body) (process_assign_exp (car body) env)))
      (else (cons (processor (car body) env) (process_when_exp_body (cdr body) env)))
      )
    )
  )

;parse-> (each-exp (assign-exp a (num-exp 0))
; (each-body-exp (bool-exp < (num-exp 5) (var-exp a)) (assign-exp a (math-exp + (var-exp a) (num-exp 1)))
; (each-list-exp (output-exp (var-exp a)))))
;>(each-list-exp (output-exp (var-exp a) (each-body-exp (bool-exp....
(define process_each_exp
  (lambda (body env)
    (let*
        ((new_env (process_assign_exp (cadr body) env))
         (condition (process_bool_exp (cadr (caddr body)) new_env))
         (true_exp (append (cadddr (caddr body)) (list (caddr body)))))
      (if condition
          (erase_void (processor true_exp new_env))
          (display-output "each exp ends here"))
     )
    )
  )
  
;when keyword is each-list-exp
;(each-list-exp (output-exp (var-exp a)) (output-exp (var-exp a)))
;(each-list-exp (output-exp (var-exp a)) (each-body-exp ...)) -> (each-list-exp #void)
(define process_each_list_exp
  (lambda (body env)
    (cond
      ((eq? (length body) 1) (display-output "each loop list exp ends here")) ;to do it won't reach here yet
      ((eq? (car (cadr body)) 'assign-exp)
       (processor (cons 'each-list-exp (cddr body)) (processor (cadr body) env)))
      ((void? (cadr body)) (list (cadr body)))
      ((and
        (eq? (car (cadr body)) 'each-body-exp)
        (eq? (length body) 2))
       (processor (cadr body) env))
      (else
       (cons (processor (cadr body) env)
             (processor
              (cons 'each-list-exp (cddr body))
              env)))
      ;when the last time each-body-exp return #void; -> (process (each-list-exp #void)) -> #void)
     )
    )
  )
    

;(each-body-exp (bool-exp < (num-exp 5) (var-exp a))
;(assign-exp a (math-exp + (var-exp a) (num-exp 1))) (each-list-exp (output-exp (var-exp a)))))
(define process_each_body_exp
  (lambda (body env)
    (let*
        ((new_env (process_assign_exp (caddr body) env))
         (condition (process_bool_exp (cadr body) new_env))
         (true_exp (append (cadddr body) (list body))))
      (if condition
          (processor true_exp new_env)
          (display-output "each-loop stops here.")
          )
      )
    )
  )

;(#<void> (#<void> (#<void> (#<void> #<void>))))
(define erase_void
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((void? lst) '())
      ((void? (car lst)) (erase_void (cdr lst)))
      (else (append (car lst) (erase_void (cdr lst))))
      )
    )
  )
                    

    

;(((a 1) (b 2) (c 3)) ((x 10) (y 5) (z 7)) ((m 6) (o 8)))
;->((a b c) (x y z) (m o))
;->(a b c x y z m o)
(define extract_varname_from_env
  (lambda (env)
    (map (lambda (scope)
           (map (lambda (pair)
                  (car pair)) scope)
           ) env
         )
    )
  )

;((a b c) (d e) (o m)) -> (m o e d a b c)
(define combine
  (lambda (lst_of_lst)
    (cond
     ((null? lst_of_lst) '())
     ((eq? (length lst_of_lst) 1) (car lst_of_lst))
     ;check if the second item is empty, if not, move the first item of the second list to first list
     ;otherwise remove the second list when it is an empty list
     ((null? (cadr lst_of_lst)) (combine (cons (car lst_of_lst) (cddr lst_of_lst))))
     (else
      (combine
       (cons (cons (car (cadr lst_of_lst)) (car lst_of_lst)) (cons (cdr (cadr lst_of_lst)) (cddr lst_of_lst)))
       )
      )
     )
    )
  )

;((a 1) (b 2) (c 3))
;(update_variable_in_scope a 5) -> ((a 5) (b 2) (c 3))
(define update_variable_in_scope
  (lambda (varname value scope)
    (cond
      ((null? scope) '())
      ((eq? (car (car scope)) varname) (cons (list varname value) (cdr scope)))
      (else (cons (car scope) (update_variable_in_scope varname value (cdr scope))))
     )
    )
  )

;check the first scope if the scope contains the varname, if yes, update scope, if not check next scope
(define update_varaible_in_env
  (lambda (varname value env)
    (cond
      ((null? env) '())
      ((is_var_in_scope varname (car env))
       (cons (update_variable_in_scope varname value (car env)) (cdr env)))
      (else (cons (car env) (update_varaible_in_env varname value (cdr env))))
     )))


(define is_var_in_scope
  (lambda (varname scope)
    (is_in_list (map (lambda (pair)
           (eq? (car pair) varname)) scope)
                true);(true false false .....)
    )
  )
                               

(define processor
  (lambda
      (parsedCode env)
    (cond
      ;when parsed Code is empty
      ((null? parsedCode) (error-output "Processor receives an illegal parsed code"))
      ;when parsed code is a var expression
      ((eq? 'var-exp (car parsedCode))
       (process_var_exp parsedCode env))
      ;when parsed code is a app expression
      ((eq? 'app-exp (car parsedCode))
       (process_app_exp parsedCode env))
      ;when parsed code is a numeric expression
      ((eq? 'num-exp (car parsedCode))
       (process_num_exp parsedCode env))
      ;when parsed code is a boolean expression
      ((eq? 'bool-exp (car parsedCode))
       (process_bool_exp parsedCode env))
      ;when parsed code is a ask expression
      ((eq? 'ask-exp (car parsedCode))
       (process_ask_exp parsedCode env))
      ;when parsed code is a math expression
      ((eq? 'math-exp (car parsedCode))
       (process_math_exp parsedCode env))
      ;when parsed code is a let expression
      ((eq? 'let-exp (car parsedCode))
       (process_let_exp parsedCode env))
      ;when parsed code is an assignment expression
      ((eq? 'assign-exp (car parsedCode))
       (process_assign_exp parsedCode env))
      ;when parsed code is a when expression
      ((eq? 'when-exp (car parsedCode))
       (process_when_exp parsedCode env))
      ((eq? 'each-exp (car parsedCode))
       (process_each_exp parsedCode env))
      ((eq? 'each-body-exp (car parsedCode))
       (process_each_body_exp parsedCode env))
      ((eq? 'each-list-exp (car parsedCode))
       (process_each_list_exp parsedCode env))
      ;when parsed code is an output expression
      ((eq? 'output-exp (car parsedCode))
       (displayln (string-append "***output***: "(number->string (processor (cadr parsedCode) env)))))
      ;when parsed code is a block expression
      ((eq? 'block-exp (car parsedCode))
       (pick_first_non_void_from_list
        (map (lambda (code) (processor code env)) (cdr parsedCode))))
      ;....
      ;otherwise
      (else (error-output "Processor failed to produce result."))
      )
    )
  )


(provide (all-defined-out))