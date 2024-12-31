#lang scheme
(require sicp)

; === EVAL and APPLY === ;
(define (EVAL exp env)
  (cond
    ((self-value? exp)  exp)
    ((variable? exp)    (lookup-variable-value exp env))
    ((quoted? exp)      (text-of-quotation exp))
    ((assignment? exp)  (eval-assignment exp env))
    ((definition? exp)  (eval-definition exp env))
    ((if? exp)          (eval-if exp env))
    ((lambda? exp)      (make-procedure (lambda-ps exp) (lambda-body exp) env))
    ((begin? exp)       (eval-sequence (begin-actions exp) env))
    ((and? exp)         (eval-and exp env))
    ((or? exp)          (eval-or exp env))
    ((cond? exp)        (EVAL (cond->if exp) env))
    ((let? exp)         (EVAL (let->combination exp) env))
    ((let*? exp)        (EVAL (let*->nested-lets exp) env))
    ((application? exp) (APPLY (EVAL (operator exp) env)
                               (map (lambda (exp) (EVAL exp env)) (operands exp))))
    (else (error "Unknown expression type! --- EVAL" exp))))

(define (APPLY procedure arguments)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment (procedure-parameters procedure)
                                        arguments
                                        (procedure-environment procedure))))
    (else (error "unknown procedure type --- APPLY" procedure))))
; === EVAL and APPLY === ;

; === Expression Checkers === ;
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-value? exp)        (or (number? exp) (string? exp)))
(define variable?                symbol?)
(define (quoted? exp)            (tagged-list? exp 'quote))
(define (assignment? exp)        (tagged-list? exp 'set!))
(define (definition? exp)        (tagged-list? exp 'define))
(define (if? exp)                (tagged-list? exp 'if))
(define (lambda? exp)            (tagged-list? exp 'lambda))
(define (begin? exp)             (tagged-list? exp 'begin))
(define (and? exp)               (tagged-list? exp 'and))
(define (or? exp)                (tagged-list? exp 'or))
(define (cond? exp)              (tagged-list? exp 'cond))
(define (let? exp)               (tagged-list? exp 'let))
(define (let*? exp)              (tagged-list? exp 'let*))
(define application?             pair?)
(define (compound-procedure? p)  (tagged-list? p 'procedure))
(define (primitive-procedure? p) (tagged-list? p 'primitive))
; === Expression Checkers === ;

; === Syntax === ;
(define assignment-variable      cadr)
(define assignment-value         caddr)
(define lambda-ps                cadr)
(define lambda-body              cddr)
(define if-predicate             cadr)
(define if-consequent            caddr)
(define begin-actions            cdr)
(define first-exp                car)
(define rest-exps                cdr)
(define operator                 car)
(define operands                 cdr)
(define cond-clauses             cdr)
(define cond-predicate           car)
(define cond-actions             cdr)
(define enclosing-environment    cdr)
(define first-frame              car)
(define frame-variables          car)
(define frame-values             cdr)
(define procedure-parameters     cadr)
(define procedure-body           caddr)
(define procedure-environment    cadddr)
(define primitive-implementation cadr)
; === Syntax === ;

; === Evaluation === ;
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env '())
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (text-of-quotation exp)
  (cadr exp))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (EVAL (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (EVAL (definition-value exp) env)
    env)
  'ok)

(define (eval-if exp env)
  (if (true? (EVAL (if-predicate exp) env))
      (EVAL (if-consequent exp) env)
      (EVAL (if-alternative exp) env)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (EVAL (first-exp exps) env))
        (else (EVAL (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-and exp env)
  (define (eval-and-clauses clauses)
    (cond ((null? clauses) true)
          ((false? (EVAL (car clauses) env)) false)
          (else (eval-and-clauses (cdr clauses)))))
  (eval-and-clauses (cdr exp)))

(define (eval-or exp env)
  (define (eval-or-clauses clauses)
    (cond ((null? clauses) false)
          ((true? (EVAL (car clauses) env)) true)
          (else (eval-or-clauses (cdr clauses)))))
  (eval-or-clauses (cdr exp)))
; === Evaluation  === ;

; === Derived Special Forms === ;
(define (let->combination exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (let ((vars (map car bindings))
          (vals (map cadr bindings)))
      (cons (cons 'lambda (cons vars body)) vals))))

(define (let*->nested-lets exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (define (expand bindings)
      (if (null? bindings)
          (cons 'begin body)
          (let ((first-binding (car bindings))
                (rest-bindings (cdr bindings)))
            (list 'let (list first-binding) (expand rest-bindings)))))
    (expand bindings)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
; === Derived Special Forms === ;

; === Conditionals === ;
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last --- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
; === Conditionals === ;

; === Sequences === ;
(define (last-exp? seq)
  (null? (cdr seq)))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))
; === Sequences === ;

; === Apply and Environment === ;
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (cons vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env '())
        (error "Unbound variable --- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? (car vars) var)
             (set-car! vals val))
            (else
             (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             '())))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'display display)
        ;; more primitives
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define the-global-environment (setup-environment))
; === Apply and Environment === ;

; === Driver Loop === ;
(define (driver-loop)
  (newline)
  (newline)
  (display ">> ")
  (let ((input (read)))
    (if (eq? input 'run-tests!!)
        (run-tests)
        (let ((output (EVAL input the-global-environment)))
          (display "")
          (user-print output)))
    (driver-loop)))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
; === Driver Loop === ;

; === Tests === ;
(define tests '())
(define test-environment (setup-environment))
(define (add-test test solution success-msg failure-msg)
  (set! tests (append tests 
                      (list (lambda () 
                              (display (if (equal? (EVAL test test-environment) solution) 
                                           success-msg 
                                           failure-msg)))))))
(define (run-tests)
  (define (run-tests-helper test-list)
    (if (null? test-list)
        "all tests done!"
        (begin
          ((car test-list))
          (newline)
          (run-tests-helper (cdr test-list)))))
  (run-tests-helper tests))
      
(add-test '(car '(1 2 3)) 1 
          "Test for 'car' passed."
          "Test for 'car' failed.")

(add-test '(cdr '(1 2 3)) '(2 3) 
          "Test for 'cdr' passed."
          "Test for 'cdr' failed.")

(add-test '(cons 1 '(2 3)) '(1 2 3) 
          "Test for 'cons' passed."
          "Test for 'cons' failed.")

(add-test '(null? '()) #t 
          "Test for 'null?' passed."
          "Test for 'null?' failed.")

(add-test '(+ 2 3) 5 
          "Test for '+' passed."
          "Test for '+' failed.")

(add-test '(* 3 4) 12 
          "Test for '*' passed."
          "Test for '*' failed.")

(add-test '(- 10 4) 6 
          "Test for '-' passed."
          "Test for '-' failed.")

(add-test '(/ 20 4) 5 
          "Test for '/' passed."
          "Test for '/' failed.")

(add-test '(< 2 5) #t 
          "Test for '<' passed."
          "Test for '<' failed.")

(add-test '(> 10 3) #t 
          "Test for '>' passed."
          "Test for '>' failed.")

(add-test '(= 4 4) #t 
          "Test for '=' passed."
          "Test for '=' failed.")

(add-test '(let ((x 10) (y 3)) (+ x y 2)) 15
          "Test for 'let' passed."
          "Test for 'let' failed.")

(add-test '(if (< 2 3) "true branch" "false branch") "true branch"
          "Test for 'if' passed."
          "Test for 'if' failed.")

(add-test '(begin (define x 42) x) 42
          "Test for 'define' passed."
          "Test for 'define' failed.")

(add-test '((lambda (a b) (* a b)) 4 5) 20
          "Test for 'lambda' passed."
          "Test for 'lambda' failed.")

(add-test '(begin (define (square x) (* x x)) (square 6)) 36
          "Test for 'define' passed."
          "Test for 'define' failed.")

(add-test '(begin 
             (define (factorial n)
               (if (= n 0)
                   1
                   (* n (factorial (- n 1)))))
             (factorial 5)) 120
                            "Test for 'recursion' passed."
                            "Test for 'recursion' failed.")

(add-test '(cond ((< 2 1) "branch 1")
                 ((= 2 2) "branch 2")
                 (else "default")) "branch 2"
                                   "Test for 'cond' passed."
                                   "Test for 'cond' failed.")

(add-test '(quote (a b c)) '(a b c)
          "Test for 'quote' passed."
          "Test for 'quote' failed.")
; === Tests === ;

(driver-loop)
