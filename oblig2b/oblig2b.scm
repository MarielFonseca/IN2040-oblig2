;;Oppgave 1a
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

;; Oppgave 1b
;; Se .pdf vedlegg

;;Oppgave 2a
;;vi antar at dette er en flat (ikke-nøstet) liste
(define (make-stack items) 

  (define (pop) 
    (if (not (null? items))        ;;sjekker om lista ikke er tom
        (set! items (cdr items)))) ;; gjør om lista til det resterende

  (define (push argumenter)
    (set! items (append (reverse argumenter) items))) ;;lager en ny liste med append
                          ;;setter inn det reverserte nye før resten av items

  (define (dispatch message . argumenter) ;; punktum for å ta et vilkårlig antall argumenter
    (cond ((eq? message 'pop!) (pop))
          ((eq? message 'push!) (push argumenter)) ;; kaller på push med argumentene
          ((eq? message 'stack) items)             ;; returnerer items direkte
          (else 'unknown)))
  dispatch)


;; Oppgave 2b

;; basically det samme som 2a, men uten innkapsling
(define (pop! objekt)
  (objekt 'pop!))

(define (stack objekt)
  (objekt 'stack))

(define (push! objekt . argumenter)
  (apply objekt 'push! argumenter))


;; Oppgave 3a
;; Se .pdf vedlegg

;; Oppgave 3b
;; Se .pdf vedlegg

;; Oppgave 3c

(define (cycle? ls)
  (cond
    ((null? ls) #f)
    (else (has-cycle ls (cdr ls)))))

(define (has-cycle slow fast) ;; tar inn forskjellige deler av samme liste
  (cond
   ((null? fast) #f)
   ((null? (cdr fast)) #f)
   ((eq? slow fast) #t) ;; hvis de noen gang overlapper, er det en cycle
   (else (has-cycle (cdr slow) (cddr fast))))) ;; hvis ingenting slår til, søk videre med resten av listen


;;Oppgave 3d
#|
Grunnen til at bar ikke blir en ordentlig liste, er fordi den er sikrulær.
Alle lister må ende med et tomt element (eller den tomme listen, '()) på slutten.
Når en liste er sirkulær på denne måten, vil ikke den tomme listen være til stedet.
På grunn av dette, er det ikke en vanlig liste.

Derfor er bah #t, fordi den har en "slutt"/ '()
Bar gir #f, fordi pekerne peker konstant på hverandre i en evig løkke
|#


;; TESTER ;;

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

(c1) ;;1
(c1) ;;2
(c1) ;;3

count ;; 42

(c2) ;; 1

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)
(s2 'pop!)

(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)

(define bah (list 'bring 'a 'towel))
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
(cycle? bar)