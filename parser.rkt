#lang racket

(define (grammar str) (
    (let ([lr (string-split str "->")]) (
        (cons (first lr) (cons (string-split (rest lr) '())))
    ))
)) 
(define (get-symbol grammar) (car grammar))
(define (get-right grammar) (cdr grammar)) 

(define (item pos dot grammar) ('(dot pos grammar)))
(define (get-dot item) (first item))
(define (get-pos item) (first (rest item)))
(define (get-grammar item) (rest (rest item)))

(define (grammar-list grammars) (map (grammar) grammars))

(define (from-symbol grammars symbol) (
  (filter (lambda (grammar) (= (get-symbol grammar) symbol))  
        (grammar-list grammars)) 
))

(define (stateset grammars) (list (list (from-symbol grammars "S"))))

(define (predict stateset input grammars) (
    (remove-duplicates
        (~a stateset 
            (map (item (get-pos (first stateset)) (get-dot first stateset)
                (from-symbol (grammar-list grammars))
                    (map (first (get-right (get-grammar))) stateset)
                )
            )
        )
    )
))

(define (in-grammar? in grammar) (
    (if (= (first grammar) '\'') 
        (= in (drop-right (rest grammar)))
        (if (= (first grammar) '\[') 
            (ormap (= in) (drop-right (rest grammar))) 
            #f
        )
    )
))
(define (inc-dot item) (item (get-pos i) (add1 (get-dot i)) (get-grammar i))) 
(define (scan stateset input grammars) (
    (let ([is-grammar (filter (lambda (item) 
            (in-grammar? (list-ref input (get-dot item)
                (split-at (get-dot item) 
                    (get-right (get-grammar item)))))) stateset
    )] (if (empty? is-grammar)
        #f
        (map (inc-dot) is-grammar)
))
     
(define (complete statesets input grammars) (
    (let ([completed (last (last statesets))]) (
))
    
    
    
     

