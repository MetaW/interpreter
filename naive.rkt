#lang racket

; this is an interpreter for a toy language R2
; according to Yin Wang's blog


; intro-1
(define (tree-sum exp)
	(match exp
		[(? number? x) x]
		[`(,x ,y) 
			(+ (tree-sum x)
			   (tree-sum y))]))

;;; tree-sum without using pattern match

;(define (tree-sum exp)
;	(cond 
;		((pair? exp) 
;			(+ (tree-sum (car exp))
;			   (tree-sum (cadr exp))))
;		(else exp)))




;intro-2
(define (calc exp)
	(match exp
		[(? number? x) x]
		[`(,op ,x ,y) 
			(let ([v1 (calc x)]
				  [v2 (calc y)])
				 (match op
				 	['+ (+ v1 v2)]
				 	['- (- v1 v2)]
				 	['* (* v1 v2)]
				 	['/ (/ v1 v2)]))]))



;;; whole interpreter
;------------------------------------------------


(define env0 '())


(define (ext-env x v env)
	(cons `(,x ,v) env))


(define (lookup-env x env)
	(if (null? env)
		(error "Undefined variable -- LOOKUP_ENV" x)
		(if (eq? x (car (car env)))
			(cadar env)
			(lookup-env x (cdr env)))))


(define (interp exp env)
	(match exp
		[(? number? x) x]			;number
		
		[(? symbol? x) 
			(lookup-env x env)]		;symbal
		
		[`(lambda (,x) ,e) 			;lambda expression
			(cons exp env)]			
		
		[`(let ([,x ,v]) ,e)		;let 
			(let ([t (interp v env)])
				(interp e (ext-env x t env)))]
		
		[`(,f ,e)					;function call
			(let ([vf (interp f env)]
				  [ve (interp e env)])
				 (match vf
				 	[(cons `(lambda (,x) ,e) env-save) 
				 		(interp e (ext-env x ve env-save))]))]
		
		[`(,op ,e1 ,e2)				;+-*/
			(let ([v1 (interp e1 env)]
				  [v2 (interp e2 env)])
				(match op
					['+ (+ v1 v2)]
					['- (- v1 v2)]
					['* (* v1 v2)]
					['/ (/ v1 v2)]))]))


(define (r2 exp)
	(interp exp env0))


;;; test

;(r2 '(+ 1 2))
; => 3

;(r2 '(* (+ 1 2) (+ 3 4)))
; => 21

;(r2 '((lambda (x) (* 2 x)) 3))
; => 6

;(r2
;'(let ([x 2])
;   (let ([f (lambda (y) (* x y))])
;     (let ([x 4])
;       (f 3)))))
; => 6





