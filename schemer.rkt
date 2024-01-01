#lang typed/racket
; (: atom? (-> Any Boolean : (! (Pair Any Any))) <-- "not a pair" is not a type that I can tell
; (: atom? (-> Any Boolean : Symbol)) ; <-- type is OK but the implementation doesn't check
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
(: lat? (-> (Listof Any) Boolean))
(define (lat? l)
    (cond
        [(null? l) #t]
        [(atom? (first l)) (lat? (rest l))]
        [else #f]))
(: member? (-> Symbol (Listof Symbol) Boolean))
(define (member? a lat)
    (cond
        [(null? lat) #f]
        [else (or (eq? (car lat) a) (member? a (rest lat)))]))
(: rember (-> Symbol (Listof Symbol) (Listof Symbol)))
(define (rember a lat)
    (cond
        [(null? lat) '()]
        [(eq? (car lat) a) (cdr lat)]
        [else (cons (car lat) (rember a (cdr lat)))]))
(: firsts (-> (Listof (Listof Any)) (Listof Any)))
(define (firsts l)
    (cond
        [(null? l) '()]
        [(null? (first l)) (firsts (rest l))]
        [else (cons (first (first l)) (firsts (rest l)))]))
(: insertR (-> Symbol Symbol (Listof Symbol) (Listof Symbol)))
(define (insertR new old lat)
    (cond
        [(null? lat) '()]
        [(eq? (car lat) old) (cons old (cons new (cdr lat)))]
        [else (cons (car lat) (insertR new old (cdr lat)))]))
(: multiRember (-> Symbol (Listof Symbol) (Listof Symbol)))
(define (multiRember a lat)
    (cond
        [(null? lat) '()]
        [(eq? (car lat) a) (multiRember a (cdr lat))]
        [else (cons (car lat) (multiRember a (cdr lat)))]))
(: multiInsertR (-> Symbol Symbol (Listof Symbol) (Listof Symbol)))
(define (multiInsertR new old lat)  
    (cond
        [(null? lat) '()]
        [(eq? (car lat) old) (cons old (cons new (multiInsertR new old (cdr lat))))]
        [else (cons (car lat) (multiInsertR new old (cdr lat)))]))
(: multiInsertL (-> Symbol Symbol (Listof Symbol) (Listof Symbol)))
(define (multiInsertL new old lat)  
    (cond
        [(null? lat) '()]
        [(eq? (car lat) old) (cons new (cons old (multiInsertL new old (cdr lat))))]
        [else (cons (car lat) (multiInsertL new old (cdr lat)))]))
(: plus (-> Positive-Integer Integer Positive-Integer))
(define (plus n m)
    (cond
        [(zero? m) n]
        [else (plus (add1 n) (sub1 m))]))
(: minus (-> Integer Integer Integer))
(define (minus n m)
    (cond
        [(zero? m) n]
        [else (minus (sub1 n) (sub1 m))]))
(: times (-> Positive-Integer Integer Integer))
(define (times n m)
    (cond
        [(zero? m) 0]
        [else (plus n (times n (sub1 m)))]))
(: addtup (-> (Listof Positive-Integer) Nonnegative-Integer))
(define (addtup tup)
    (cond
        [(null? tup) 0]
        [else (plus (car tup) (addtup (cdr tup)))]))
(: tup+ (-> (Listof Positive-Integer) (Listof Positive-Integer) (Listof Positive-Integer)))
(define (tup+ tup1 tup2)
    (cond
        [(and (null? tup1) (null? tup2)) '()]
        [(null? tup1) tup2]
        [(null? tup2) tup1]
        [else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))]))
(: gt (-> Nonnegative-Integer Nonnegative-Integer Boolean))
(define (gt n m)
    (cond
        [(zero? n) #f]
        [(zero? m) #t]
        [else (gt (sub1 n) (sub1 m))]))
(: div (-> Integer Integer Integer))
(define (div n m)
    (cond
        [(< n m) 0]
        [else (add1 (div (- n m) m))]))
(: no-nums (-> (Listof (U Number Symbol)) (Listof Symbol)))
(define (no-nums lat)
    (cond
        [(null? lat) '()]
        [(number? (car lat)) (no-nums (cdr lat))]
        [else (cons (car lat) (no-nums (cdr lat)))]))
(define-type LispTree (Listof (U Symbol LispTree)))
(: rember* (-> Symbol LispTree LispTree))
(define (rember* a l)
    (cond
        [(null? l) '()]
        [(not (pair? (car l))) ;(symbol? (car l))
         (cond
            ; [(null? (car l)) (cons (rember* a (car l)) (rember* a (cdr l)))] ; <-- not needed because
            ; (remember a '()) is '() -- the identity. It would be needed for any function that isn't the identity for l when l='()
            [(eq? (car l) a) (rember* a (cdr l))]
            [else (cons (car l) (rember* a (cdr l)))] )]
        [else (cons (rember* a (car l)) (rember* a (cdr l)))]))
(: subst* (-> Symbol Symbol LispTree LispTree))
(define (subst* old new l)
    (cond
        [(null? l) '()]
        [(not (pair? (car l)))
         (cond
            [(eq? (car l) old) (cons new (subst* old new (cdr l)))]
            [else (cons (car l) (subst* old new (cdr l)))] )]
        [else (cons (subst* old new (car l)) (subst* old new (cdr l)))]))
(: insertL* (-> Symbol Symbol LispTree LispTree))
(define (insertL* new old l)
    (cond
        [(null? l) '()]
        [(not (pair? (car l)))
         (cond
            [(eq? (car l) old) (cons new (cons old (cdr l)))]
            [else (cons (car l) (insertL* new old (cdr l)))] )]
        [else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))]))
(: leftmost (-> LispTree Symbol))
(define (leftmost l)
    (cond
        [(null? l) (car l)]
        [else (let ((fst (car l)))
                (cond
                    [(symbol? fst) fst]
                    [(pair? fst) (leftmost fst)]
                    [else (error 'crash)]))]))
        ; [(pair? (car l)) (leftmost (car l))]
        ; [else (car l)]))
(define-type AExp (U Integer Symbol (List AExp (U '+ '- '^) AExp)))
(: numbered (-> AExp Boolean))
(define (numbered aexp) 
    (if (atom? aexp)
        (number? aexp)
        (and (numbered (car aexp)) (numbered (caddr aexp)))))
(: value (-> AExp Number))
(define (value aexp)
    (cond
        [(number? aexp) aexp]
        [(symbol? aexp) (error 'no-symbols-allowed)]
        [(eq? (cadr aexp) '+) (+ (value (car aexp)) (value (caddr aexp)))]
        [(eq? (cadr aexp) '-) (- (value (car aexp)) (value (caddr aexp)))]
        [(eq? (cadr aexp) '^) (expt (value (car aexp)) (value (caddr aexp)))] ; TODO: Figure out how to coerce to Integer here (ann assert cast #{: x T} are the methods)
        [else (error 'crash)]))
; TODO: Figure out how cast (unsafely?)
(define-type ArithExp (U Integer Symbol Op))
(struct Op ([l : ArithExp] [op : (U '+ '- '^)] [r : ArithExp]))
(: exp-numbered? (-> ArithExp Boolean))
(define (exp-numbered? e)
    (match e
        [(Op l _ r) (and (exp-numbered? l) (exp-numbered? r))]
        [n (number? n)]
    ))
; (exp-numbered? 'a)
; (exp-numbered? (Op 'a '+ 'b))
; (exp-numbered? (Op 1 '+ 2))
; (exp-numbered? (Op 1 '+ (Op 2 '+ 3)))
; (exp-numbered? (Op 1 '+ (Op 'a '+ 3)))
(: set? (-> (Listof Symbol) Boolean))
(define (set? lat)
    (match lat
        ['() #t]
        [(cons a lat) (and (not (member? a lat)) (set? lat))]))
(: makeset (-> (Listof Symbol) (Listof Symbol)))
(define/match (makeset _)
    [('()) '()]
    [((cons a l)) (cons a (makeset (multiRember a l)))])
; (makeset '())
; (makeset '(a))
; (makeset '(a b))
; (makeset '(a b a))
(: subset? (-> (Listof Symbol) (Listof Symbol) Boolean))
(define (subset? set1 set2)
    (match set1
        ['() #t]
        [(cons a s1) (and (member? a set2) (subset? s1 set2))]))
; (subset? '()'())
; (subset? '(a) '())
; (subset? '(a) '(a))
; (subset? '(a) '(a b))
(: eqset? (-> (Listof Symbol) (Listof Symbol) Boolean))
(define (eqset? set1 set2)
    (and (subset? set1 set2) (subset? set2 set1)))
(: intersect (-> (Listof Symbol) (Listof Symbol) (Listof Symbol)))
(define (intersect set1 set2)
    (match set1
        ['() '()]
        [(cons a s1) #:when (member? a set2) (cons a (intersect s1 set2))]
        [(cons _ s1) (intersect s1 set2)]))
(: intersectall (-> (Listof (Listof Symbol)) (Listof Symbol)))
(define (intersectall l)
    ;(foldl intersect (car l) (cdr l)) also works
    (match l
        [(cons s '()) s]
        [(cons s l) (intersect s (intersectall l))]))
(intersect '(a) '(b))
(intersect '() '(a))
(intersect '(a b) '(b c))
(intersect '(a b) '(b a))
(intersectall '((a b) (b c) (a b c) (t b a)))