#lang racket
; meta.rkt
; The metacircular evaluator from section 4.1

(define (seval exp environ)
  ; Evaluate a scheme expression
  (cond ((primitiva? exp) exp)                            ; Primitive just "are". Return back
        ((simbolo? exp) (busca-var exp environ))  ; Symbols? Look up in the environment.
        ((define? exp) (seval-define exp environ)) ;Ya la dan hecha
        ((if? exp) (seval-if exp environ)) ;Ya la dan hecha
        ((quote? exp) (seval-quote exp environ)) ;Ya la dan hecha
        ; ((cond? exp) ...)
        ; ((let ...))
        ; ((delay...))
        ((begin? exp) (seval-begin exp environ)) ;Implementado
        ((lambda? exp) (seval-lambda exp environ)) ;Implementado
        ((aplicacion-procedimiento? exp) (aplicar-proc exp environ)) ;Se basa en poco más que mapear una lista jerárquica como en los ejers 2.27 y 2.28 del capítulo 2
  (else (error "Error desconocido"))
        )
  )


;Ambiente
(define (busca-var nombre ambiente)
  (if (hash-has-key? ambiente nombre)
      (hash-ref ambiente nombre)
      (display "Symbol lookup failed")
      ))

(define (anhadir-a-ambiente nombre-var valor-var ambiente)
  (hash-set! ambiente nombre-var valor-var))

(define (extender-ambiente keys valores parent-env)
  (let ((nuevo (hash-copy parent-env)))
    (for-each (lambda (key val)
                (anhadir-a-ambiente key val nuevo))
              keys valores)
    nuevo))


;Abstracciones de datos
(define (primitiva? exp)
  (or (number? exp) (boolean? exp)))

(define (aplicacion-procedimiento? exp)
  (list? exp))

(define (simbolo? exp)
  (symbol? exp))

(define (define? exp) ; Predicate to test
  (and (list? exp) (eq? (car exp) 'define))
  )

(define (lambda? exp)
  (and (eq? (car exp) 'lambda) (list? exp)))

; Selectors to extract information from the expression
(define (define-name exp)
  (cadr exp)
)

(define (define-value exp)
  (caddr exp)
  )

(define (operator exp) (car exp)) ; Del libro

(define (operands exp) (cdr exp)) ; Del libro


; Evaluadores
(define (seval-define exp environ)
  (let ((name (define-name exp))
        (value (define-value exp)))
    (anhadir-a-ambiente name (seval value environ) environ)
    )
  )

(define (seval-lambda exp env)
  (make-procedure exp env))

(define (seval-begin exp environ)
  (define (mapeo-func arbol exp environ) ;Recorrido árbol
    (if (not (null? (cdr arbol)))        
        (begin (seval (car arbol) environ) ; Profundidad
               (mapeo-func (cdr arbol) environ))
        (seval (car arbol) environ) ; Hoja / Fin de rama)
    ))  
  (mapeo-func (begin-expressions exp) exp environ))


;Cosas quote
(define (quote? exp)
  (and (list? exp) (eq? (car exp) 'quote)))

(define (quote-expression exp)
  (cadr exp))


; Como evaluar el operador quote

(define (seval-quote exp environ)
  (quote-expression exp)
  )

; (if test consequence alternative)

(define (if? exp)
  (and (list? exp) (eq? (car exp) 'if)))

; Selectors
(define (if-test exp)
  (cadr exp)
  )

(define (if-consequence exp)
  (caddr exp)
  )

(define (if-alternative exp)
  (cadddr exp)
  )

; como evaluar los if
(define (seval-if exp environ)
  (if (seval (if-test exp) environ)        ;  Evaluate the test first
      (seval (if-consequence exp) environ)
      (seval (if-alternative exp) environ)
      )
  )

; (begin exp1 ... expn)
; Evaluar todas las expresiones

(define (begin? exp)
  (and (list? exp) (eq? (car exp) 'begin))
  )

(define (begin-expressions exp)
  (cdr exp)      ; Note: this returns a *list* of the expressions
  )

;Caso especial procedimiento (y lambda)
(define (aplicar-proc exp environ) ;Esta funcion se basa en el mapeado de una lista jerárquica.
  (let ((proc (mapear-proc exp environ))) 
  (if (procedure? (car proc))
      (apply (car proc) (cdr proc)) ; Primitivo
      (seval-begin (cons 'begin (caddr (car proc))) ; Compuesto
                      (extender-ambiente (cadr (car proc))
                                         (cdr proc)
                                         (cadddr (car proc)))))))

(define (mapear-proc exp environ) ;Mapeado en cuestión, similar a los ejers 2.27 y 2.28
  (cons
   (seval (car exp) environ)
   (map (lambda (e) (seval e environ)) (cdr exp))))

;Procedimiento (copiado del libro)
(define (make-procedure exp environ)
  (list 'procedure (cadr exp) (cddr exp) environ))


;defining the environment
(define environ (make-hash))
(hash-set! environ '+ +)
(hash-set! environ '- -)
(hash-set! environ '= =)
(hash-set! environ '> >)
(hash-set! environ '< <)
(hash-set! environ '* *)
(hash-set! environ '/ /)


(define (check-equal? actual expected message)
  (if (equal? actual expected)
      (display "Todo bien \n")
      (error message)))

;; Varias pruebas para ver que es lo que tiene que ocurrir
(check-equal? (seval '42 environ) 42 "Primitives failed")
(hash-set! environ 'foo 123)
(check-equal? (seval 'foo environ) 123 "Symbol lookup failed")
(seval '(define x 42) environ)
(check-equal? (seval 'x environ) 42 "Simple define failed")
(seval '(define y (+ 2 3)) environ)
(check-equal? (seval 'y environ) 5 "Expression define failed")
(check-equal? (seval '(quote x) environ) 'x "Quoting failed")

(check-equal? (seval '(if (< 2 3) 1 (/ 1 0)) environ) 1 "if-true failed")
(check-equal? (seval '(if (< 3 2) (/ 1 0) 1) environ) 1 "if-false failed")

; Procedures
(seval '(define square (lambda (x) (* x x))) environ)
(check-equal? (seval '(square 4) environ) 16 "square failed")

(seval '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) environ)
(check-equal? (seval '(fact 5) environ) 120 "fact failed")