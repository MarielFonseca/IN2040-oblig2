;;Oppgave 1a
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))


(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

(c1) ;;1
(c1) ;;2
(c1) ;;3

count ;; 42

(c2) ;; 1

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

;; tester ;;
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


;; Oppgave 2b

;; basically det samme som 2a, men uten innkapsling
(define (pop! objekt)
  (objekt 'pop!))

(define (stack objekt)
  (objekt 'stack))

(define (push! objekt . argumenter)
  (objekt 'push! argumenter))

;; tester ;;
(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)


;; Oppgave 3a
