;; MEDLEMMER I GRUPPEN:
;; Kany Gilly Sleyman - kanygs
;; Mariel Tavares Fonseca - marieltf

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit branch)
  (if (= bit 0) 
      (left-branch branch)
      (right-branch branch)))
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree
   (make-code-tree
    (make-leaf 'fight 6)
    (make-leaf 'ninjas 5))
   (make-code-tree
    (make-leaf 'samurais 4)
    (make-code-tree
     (make-leaf 'night 2)
     (make-leaf 'by 1)))))

(define sample-code '(1 0 0 0 0 1 1 1 1 1 1 0))


;; oppgave 1a

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;; tester
(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))


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

(define foo (list 21 + 21)) ;; tester
(define baz (list 21 list 21))
(define bar (list 84 / 2))
(infix-eval foo) 
(infix-eval baz) 
(infix-eval bar)


;; oppgave 1d

;; resultatet blir en feilmelding, 'not a procedure'.
;; dette er fordi '() behandler hvert element som konstanter uten å evaluere,
;; mens list funksjonen evaluerer hvert element før den lager en liste
;; altså med '() blir elementene sett på som symboler mens i list
;; evalueres det til variabler
;; '(+ 1 2) -> (+ 1 2) mens (list (+ 1 2)) -> (3)


;;oppgave 2 a
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

;;test code
(decode sample-code sample-tree)

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
         (if (equal? symbol (symbol-leaf tree))
             '()
             #f))
        (else
         (let ((left-code
                (encode-symbol symbol (left-branch tree))))
           (if left-code
               (cons 0 left-code)
               (let ((right-code
                      (encode-symbol symbol (right-branch tree))))
                 (if right-code
                     (cons 1 right-code)
                     ("Symbol is not in the tree"))))))))


(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)

;; oppgave 2d


;; oppgave 2e
