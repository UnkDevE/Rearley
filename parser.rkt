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

(define (stateset grammars) (list (list
    (filter (lambda (grammar) (= (get-symbol grammar) "S"))  
        (grammar-list grammars)) 
)))

(define (predict stateset input grammars) (
    (remove-duplicates
        (~a stateset 
            (map (item (get-pos (first stateset)) (get-dot first stateset))
                (filter (lambda (grammar) 
                    (= (get-symbol grammar) 
                        (map (first (get-right (get-grammar))) stateset))) 
                    (grammar-list grammars)
                )
            )
        )
    )
)

(define (in-grammar? in grammar) (
    (if (= (first grammar) '\'') 
        (= in (drop-right (rest grammar)))
        (if (= (first grammar) '\[') 
            (ormap (= in) (drop-right (rest grammar))) 
            #f
        )
    )
)
(define (scan stateset input grammars) (
    (map (in-grammar? map (first (get-right (get-grammar))) stateset)
     
    
    
     

