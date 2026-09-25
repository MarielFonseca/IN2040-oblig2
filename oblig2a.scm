;; MEDLEMMER I GRUPPEN:
;; Kany Gilly Sleyman - kanygs
;; Philip Vestvik Knudsen - philipvk
;; Mariel Tavares Fonseca - marieltf


;; Oppgave 1

;; a.

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;; b

(define foo 42)

((lambda (x y)
   (if (= x y)
       'same
       'different))
 5 foo)

;; FEIL:
#|((lambda (bar baz)
   (lambda (x y)
     (foo)
     (list foo bar)))
foo 'towel
 )

((lambda (bar baz)
   (lambda (x y)
     (list foo bar)baz))
foo 'towel
 )

|#

;; c

(define (infix-eval exp) 
  (let ((operand1  (car exp))
        (operator (cadr exp))
        (operand2 (caddr exp)))
    (operator operand1 operand2)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo) 
(infix-eval baz) 
(infix-eval bar)
