;; MEDLEMMER I GRUPPEN:
;; Kany Gilly Sleyman - kanygs
;; Mariel Tavares Fonseca - marieltf

;; oppgave 1a

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))


;; oppgave 1b

;;utrykkene evaluerer til:
;;foo = 5
;;x = 42

(define foo 42)

((lambda (x y)
   (if (= x y)
       'same
       'different)) ;; evaluerer til 'different'
 5 foo)


((lambda (bar baz)
   ((lambda  (x y)
      (list y x))
    (list bar baz) baz))
 foo 'towel) ;; evaluerer til (towel (42 towel))


;; oppgave 1c

(define (infix-eval exp) 
  (let ((operand1  (car exp)) ;; henter ut hvert element
        (operator (cadr exp)) ;; og legger det til en variabel
        (operand2 (caddr exp)))
    (operator operand1 operand2))) ;; evaluerer med prefix notasjon


;; oppgave 1d

;; resultatet blir en feilmelding, 'not a procedure'.
;; dette er fordi '() behandler hvert element som konstanter uten å evaluere,
;; mens list funksjonen evaluerer hvert element før den lager en liste
;; altså med '() blir elementene sett på som symboler mens i list
;; evalueres det til variabler
;; '(+ 1 2) -> (+ 1 2) mens (list (+ 1 2)) -> (3)


;;oppgave 2a
(define (decode bits tree)
  (define (decode-1 bits current-branch acc)
    (if (null? bits)
        acc
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (cons (symbol-leaf next-branch) acc))
              (decode-1 (cdr bits) next-branch acc)))))
  (reverse (decode-1 bits tree '())))


;; oppgave 2b
;; resultatet blir: (samurais fight ninjas by night)


;; oppgave 2c
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         (if (eq? symbol (symbol-leaf tree))
             '()
             (error "Symbol is not in the tree" symbol)))
        ((memq symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((memq symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else
         (error "Symbol is not in the tree" symbol))))


;; oppgave 2d

(define (grow-huffman-tree freqs)
  (define (growing-tree trees)
    (cond ((null? (cdr trees))  ;;base case: when there is only one element left, the root
           (car trees))
          (else (let* ((left (car trees))
                       (right (cadr trees))
                       (subtree (make-code-tree left right))
                       (remaining (cddr trees)))               
                  (growing-tree (adjoin-set subtree remaining))))))
  (growing-tree (make-leaf-set freqs)))


;; oppgave 2e
#|
1.
ninjas fight: 6 bits
ninjas fight ninjas: 9 bits
ninjas fight samurais: 7 bits
samurais fight: 4 bits
samurais fight ninjas: 7 bits
ninjas fight by night: 14 bits

2.
45 / 6 = 7.5
Gjennomsnittelig lengde på kodeordene gitt er 7.5 bits

3.
For å regne hvor langt hver kodeord blir med fixed-length, brukte vi:
log_b n, hvor b er antall unike symboler/bits,
og n er antall bokstaver i alfabetet.
Vi fikk derfor log_2 16 = 4
Det betyr at hvert ord vil fast ha 4 bits.

Med fixed length:
ninjas fight: 8 bits
ninjas fight ninjas: 12 bits
ninjas fight samurais: 12 bits
samurais fight: 8 bits
samurais fight ninjas: 12 bits
ninjas fight by night: 16 bits

På gjennomsnitt, vil fixed-length ha lengere bitstreng for å formulere samme setning. 
|#


;; oppgave 2f

(define (huffman-leaves tree) ;; traversere hele treet, finne løvnoder og legg løvnodene i en acc (liste)
  (define (huff-leaves subtree acc)
    (if (leaf? subtree)
        (cons (list (symbol-leaf subtree)
                    (weight-leaf subtree)) acc)
        (let ((right-result (huff-leaves (right-branch subtree) acc)))
          (huff-leaves (left-branch subtree) right-result))))  ;;legger til resultatet fra venstre side inn til høyre side
  (huff-leaves tree '()))