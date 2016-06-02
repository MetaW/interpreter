#lang racket


(define (tree-sum exp)
	(match exp
		[(? number? x) x]
		[`(,x ,y) 
			(+ (tree-sum x)
			   (tree-sum y))]))



(define (tree-sum exp)
	(cond 
		((pair? exp) 
			(+ (tree-sum (car exp))
			   (tree-sum (cadr exp))))
		(else exp)))


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