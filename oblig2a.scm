;; MEDLEMMER I GRUPPEN:
;; Kany - kanygs
;; Philip - philipvk
;; Mariel - marieltf


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


((lambda (bar baz)
   (lambda (bar)
     (foo)
     (list foo bar)))
42 'towel
 )
