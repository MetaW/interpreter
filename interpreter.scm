
; this is a stronger interpreter 
; reference: Chapter 4.1 of SICP



;eval apply loop
(define (eval exp env)
  (cond 
        ((self-evaluating? exp) exp)

  		((variable? exp) (lookup-var-value exp env))
  		
        ((quoted? exp) (text-of-quotation exp))
  		
        ((assignment? exp) (eval-assignment exp env))
  		
        ((definition? exp) (eval-definition exp env))
  		
        ((if? exp) (eval-if exp env))
  		
        ((lambda? exp)
  			(make-proc (lambda-params exp)
  					   (lambda-body exp)
  					    env))
  		
        ((begin? exp) 
  			(eval-sequence (begin-actions exp) env))
  		
        ((cond? exp) (eval (cond-to-if exp) env))
  		
        ((application? exp)
  			(my-apply (eval (operator exp) env)
  				   (list-of-values (operands exp) env)))
  		
        (else (error "Unknow expression type -- EVAL" exp))))



; apply is a peimitive function in scheme, 
; so we use "my-apply" here in order not to override it.
(define (my-apply proc args)
  (cond 
        ((primitive-proc? proc)
  			(apply-primitive-proc proc args))
  		((compound-proc? proc)
  			(eval-sequence (proc-body proc)
  						   (extend-env  (proc-params proc)
  						   				args
  						   				(proc-env proc))))
  		(else (error "Unknow procedual type -- MY-APPLY" proc))))

; helper

(define (primitive-proc? p)
    (tagged-list? p 'primitive))

(define (apply-primitive-proc proc args)
    (apply (primitive-implementation proc) args))

(define (compound-proc? proc)
    (tagged-list? proc 'procedual))


; functions for recongnizing and dispatching
;--------------------------------------------------------------

;-------------------- self-evaluating 

; recongnize
(define (self-evaluating? exp)
    (cond ((number? exp) true)
          ((string? exp) true)
          (else false)))

; dispatch

;none



;-------------------- variable

;recongnize
(define (variable? exp) 
    (symbol? exp))

;dispatch
;(define (lookup-var-value exp env)  
;-- see environment part




;-------------------- quote

;recongnize

(define (quoted? exp)
    (tagged-list? exp 'quote))


;dispatch
(define (text-of-quotation exp)
    (cadr exp))




;-------------------- assignment

;recongnize

(define (assignment? exp)
    (tagged-list? exp 'set!))


;dispatch
(define (eval-assignment exp env)
  (set-variable-avlue! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'OK)

;helper
(define (assignment-variable exp)
    (cadr exp))

(define (assignment-value exp)
    (caddr exp))





;-------------------- definition

; recongnize
(define (definition? exp)
    (tagged-list? exp 'define))


;dispatch
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

;helper
(define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

(define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)    ;parameters
                     (cddr exp))))  ;body wraped with brackets





;-------------------- if

; recongnize

(define (if? exp)
    (tagged-list? exp 'if))

; dispatch
(define (eval-if exp env)
  (if (true? (eval (if-perdicate exp) env))
      (eval (if-consequence exp) env)
      (eval (if-alternate exp) env)))

; helper

(define (if-perdicate exp)
    (cadr exp))

(define (if-consequence exp)
    (caddr exp))

(define (if-alternate exp)
    (if (null? (cdddr exp))
        false
        (cadddr exp)))

(define (true? exp)
    (not (eq? exp false)))

(define (false? exp)
    (eq? exp false))




;-------------------- lambda

; recongnize
(define (lambda? exp)
    (tagged-list? exp 'lambda))

; dispatch
(define (make-proc params body env)
    (list 'procedual params body env))  ;data structure for compound proc


; helper
(define (lambda-params exp)
    (cadr exp))

(define (lambda-body exp)
    (cddr exp))

(define (make-lambda params body)
    (cons 'lambda (cons params body)))






;-------------------- begin

;recongnize
(define (begin? exp)
    (tagged-list? exp 'begin))

;dispatch
(define (eval-sequence exps env)
  (cond 
        ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; helper

(define (begin-actions exp)
    (cdr exp))

(define (last-exp? exp)
    (null? (cdr exp)))

(define (first-exp exp)
    (car exp))

(define (rest-exps exp)
    (cdr exp))






;-------------------- cond

; recongnize

(define (cond? exp)
    (tagged-list? exp 'cond))
; dispatch

; pass

; helper

(define (cond-clauses exp)
    (cdr exp))

(define (cond-else-clause? exp)
    (tagged-list? exp 'else))


(define (cond-actions exp)
    (cdr exp))

(define (cond-to-if exp)
    (expand-clause (cond-clauses exp)))

(define (expand-clause exp)
    (if (null? exp)
        'false
        (let ((first (car exp))
              (rest (cdr exp)))
             (if (cond-else-clause? first 'else)
                 (if (null? rest)   
                     (sequence-to-exp (cond-actions first))
                     (error "ELSE clause is not last -- COND -> IF" exp))
                 (make-if (car first)
                          (cond-actions first)
                          (expand-clause rest))))))

(define (make-if predicate consequence alternate)
    (list 'if predicate consequence alternate))

(define (sequence-to-exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (car seq))
          (else (make-begin seq))))

(define (make-begin exp)
    (cons 'begin exp))






;-------------------- application

;recongnize

(define (application? exp)
    (pair? exp))


; dispatch
; see "my-apply"


; environment
;--------------------------------------------------------------
;env : (frame frame frame ...)
;         |
;         |
;    ((var var ...) (val val ...))

(define the-empty-env '())

(define (first-frame env)
    (car env))

(define (enclosing-env env)
    (cdr env))

;frame

(define (make-frame variables values)
    (cons variables values))

(define (frame-variables f)
    (car f))

(define (frame-values f)
    (cdr f))

(define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))


(define (extend-env vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (error "Number of vars and vals are unequal -- extend-env" vars vals)))



(define (lookup-var-value var env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) 
                        (env-loop (enclosing-env env)))
                  ((eq? (car vars) var) 
                        (car vals))
                  (else (scan (cdr vars) (cdr vals)))))

        (if (null? env)
            (error "Unbounded variable -- LOOKUP-VAR" var)
            (let ((vars (frame-variables (first-frame env)))
                  (vals (frame-values (first-frame env))))
                 (scan vars vals))))
    (env-loop env))


(define (set-variable-avlue! var val env)
    (define (env-loop env)
        (define (scan vars vals)
            (cond ((null? vars) 
                        (env-loop (enclosing-env env)))
                  ((eq? (car vars) var) 
                        (set-car! vals val))
                  (else (scan (cdr vars vals)))))

        (if (null? env)
            (error "Unbounded variable -- SET-VAR" var)
            (let ((vars (frame-variables (first-frame env)))
                  (vals (frame-values (first-frame env))))
                 (scan vars vals))))
    (env-loop env))


(define (define-variable! var val env)
    (let ((frame (first-frame env)))
         (define (scan vars vals)
            (cond ((null? vars) (add-binding-to-frame! var val frame))
                  ((eq? var (car vars)) (set-car! vals val))
                  (else (scan (cdr vars) (cdr vals)))))
         (scan (frame-variables frame)
               (frame-values frame))))



; other help functions
;--------------------------------------------------------------

;check tag
(define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


(define (operator exp)
    (car exp))

(define (operands exp)
    (cdr exp))

(define (no-operands? exp)
    (null? exp))

(define (first-operand exp)
    (car exp))

(define (rest-operands exp)
    (cdr exp))


(define (proc-params p)
    (cadr p))

(define (proc-body p)
    (caddr p))

(define (proc-env p)
    (cadddr p))



(define (primitive-implementation proc)
    (cadr proc))



; init function
;---------------------------------------------------------------

(define (setup-env)
    (let ((initial-env (extend-env (primitive-proc-names)
                                   (primitive-proc-objects)
                                    the-empty-env)))
         (define-variable! 'true true initial-env)
         (define-variable! 'false false initial-env)
         initial-env))

;!!!
(define primitive-proc
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          (list '+ +)
          (list '- -)
          (list '* *)
          (list '/ /)
          (list 'display display)
          (list 'list list)
          ; other primitive proc ...
          ))

(define (primitive-proc-names)
    (map car primitive-proc))

(define (primitive-proc-objects)
    (map (lambda (p) (list 'primitive (cadr p)))    ;data structure for primitive proc
         primitive-proc))


(define the-global-env (setup-env))


;REPL
;---------------------------------------------------------------

(define input-prompt ">INPUT:")

(define output-prompt ">OUTPUT:")

(define (drive-loop) ;!!!
    (prompt-for-input input-prompt)
    (let ((in (read)))
         (let ((out (eval in the-global-env)))
              (announce-output output-prompt)
              (user-print out)))
    (drive-loop))


(define (prompt-for-input str)
    (newline) (newline) (display str) (newline))

(define (announce-output str)
    (newline) (display str) (newline))

(define (user-print out)
    (if (compound-proc? out)
        (display (list 'compound-proc
                        (proc-params out)
                        (proc-body out)
                        '<procedual-env>))
        (display out)))



