#lang racket

(define (grammar str) 
    (let ([lr (string-split str "->")]) (
        (vector (first lr) (string-split (rest lr))) 
    ))
) 
(define (get-symbol grammar) (vector-ref 0 grammar))
(define (get-right grammar) (vector-ref 1 grammar)) 

(define (item pos dot grammar) (vector dot pos grammar))
(define (get-dot item) (vector-ref 0 item))
(define (get-pos item) (vector-ref 1 item))
(define (get-grammar item) (vector-ref 2 item))

(define (grammar-list grammars) (map (grammar) grammars))

(define (from-symbol grammars symbol) 
  (filter (lambda (grammar) (= (get-symbol grammar) symbol))  
        (grammar-list grammars)) 
)

(define (statesets-start grammars) (list (list (from-symbol grammars "S"))))

(define (nullables-start grammars) 
    (filter (lambda (grammar) (= null (get-right grammar))) (grammar-list grammars))
)
(define (nullables grammars)
    (filter 
        (lambda (grammar)
            (or 
                (memq grammar (nullables-start grammars))
                (andmap (lambda (symbol) 
                            (memq (from-symbol grammars symbol) 
                                (nullables-start grammars))
                        )
                   (get-right grammar)
                )
            )
        ) (grammar-list grammars)
    )
)
(define (predict stateset input grammars) 
    (remove-duplicates
        (append 
            (map (lambda (i) 
                (let ([g (grammar (from-symbol (grammar-list grammars))
                        ((get-dot i) (get-right (get-grammar i))))])
                    (item (get-pos (stateset)) 
                        (if (memq g (nullables grammars)) 
                            (add1 (get-dot i))
                            (get-dot i)
                        ) g 
                    )
                )
            ) stateset)
        )
    )
)

(define (is-term? in grammar) 
    (if (= (first grammar) "'") 
        (= in (drop-right (rest grammar)))
        (if (= (first grammar) "[") 
            (ormap (= in) (drop-right (rest grammar))) 
            #f
        )
    )
)
(define (inc-dot i) (item (get-pos i) (add1 (get-dot i)) (get-grammar i))) 
(define (scan stateset input grammars) 
    (let ([items (filter (lambda (item) 
            (is-term? (list-ref input (get-dot item)
                (drop (get-right (get-grammar item)
                    (get-dot item)))))) stateset
    )]) (if (empty? items)
            #f
            (map (inc-dot) items)
        )
    )
)

     
(define (pos-to i pos) (item pos (get-dot i) (get-grammar i)))
(define (complete statesets input grammars) 
    (let ([completed (last (last statesets))]) (
        (append (filter (lambda (item) 
                    (= (list-ref (get-right (get-grammar item)) (get-dot item)) 
                        (get-symbol (get-grammar completed)))) 
                            (list-ref statesets (get-pos completed)))
            (map (lambda (item) (pos-to item (length statesets)))
                    (from-symbol (get-symbol (get-grammar completed)) grammars))
           (first statesets)
        )
    ))
)

(define (update-end ls item) (cons item (drop ls 1)))
(define (inner statesets input grammars) 
    (
        (map (lambda (item) 
                (let ([afterdot (drop (get-right (get-grammar item)) (get-dot item))])
                    (cond 
                        [(null? afterdot) (cons (complete statesets input grammars) statesets)]
                        [(is-term? afterdot) (inner
                            (update-end statesets (scan (first statesets) input grammars))
                            input grammars)]
                        [else (inner
                            (update-end statesets (predict (first statesets) input grammars)) 
                            input grammars)]
                    )
                )
            ) (first statesets)
        ) 
    )
)
(define (outer statesets input grammars) 
    (let ([nstatesets (inner statesets input grammars)])
        (cond 
            [(= (string-length input) (get-dot (first (first nstatesets)))) nstatesets]
            [else (outer nstatesets input grammars)]
        )
    )
)
     
(define (parse input grammars)
    ((outer (statesets-start grammars) input grammars))
)
