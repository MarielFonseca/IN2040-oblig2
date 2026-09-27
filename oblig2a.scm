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
 5 foo) ;; evaluerer til 'different'


((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz ))
 foo 'towel) ;; evaluerer til (towel (42 towel))


;; c

(define (infix-eval exp) 
  (let ((operand1  (car exp)) ;; henter ut hvert element
        (operator (cadr exp)) ;; og legger det til en variabel
        (operand2 (caddr exp)))
    (operator operand1 operand2))) ;; evaluerer med prefix notasjon

(define foo (list 21 + 21)) ;; tester
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo) 
(infix-eval baz) 
(infix-eval bar)


;; d

;; resultatet blir en feilmelding, 'not a procedure'.
;; dette er fordi '() behandler hvert element som konstanter uten å evaluere,
;; mens list funksjonen evaluerer hvert element før den lager en liste
;; altså med '() blir elementene sett på som symboler mens i list
;; evalueres det til variabler
;; '(+ 1 2) -> (+ 1 2) mens (list (+ 1 2)) -> (3)












