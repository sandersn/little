#lang typed/racket
; (: atom? (-> Any Boolean : (! (Pair Any Any))) <-- "not a pair" is not a type that I can tell
; (: atom? (-> Any Boolean : Symbol)) ; <-- type is OK but the implementation doesn't check
(define (atom? x)
    (and (not (pair? x)) (not (null? x))))
(: lat? ((Listof Any) -> Boolean))
(define (lat? l)
    (cond
        [(null? l) #t]
        [(atom? (first l)) (lat? (rest l))]
        [else #f]))
(: member? (All (T) (-> T (Listof T) Boolean)))
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
(: set? (-> (Listof Any) Boolean))
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
(define-type (Pair2 T U) (List T U))
(: a-pair? (-> Any Boolean : (Pair2 Any Any)))
(define (a-pair? x)
    (cond
        [(atom? x) #f]
        [(null? x) #f]
        [(null? (cdr x)) #f]
        [(and (pair? (cdr x)) (null? (cdr (cdr x)))) #t]
        [else #f]))
(: first2 (-> (Pair2 Any Any) Any))
(define first2 car)
(: second2 (All (T U) (-> (Pair2 T U) U)))
(define (second2 p) (cadr p))
(: build2 (-> Any Any (Pair2 Any Any)))
(define (build2 s1 s2) (list s1 s2))
(define-type (Rel T U) (Listof (Pair2 T U)))
(: fun? (-> (Rel Any Any) Boolean))
(define (fun? rel)
    (set? (firsts rel)))
; (: revrel (All (T U) (-> (Rel T U) (Rel U T))))
(: revrel (-> (Rel Any Any) (Rel Any Any)))
(define (revrel rel)
    (match rel
        ['() '()]
        [(cons (list f s) rel) (cons (build2 s f) (revrel rel))]))
    ; (map (lambda (p) (build2 (second2 p) (first2 p))) rel))
(: rember-f (All (T) ((T T -> Boolean) T (Listof T) -> (Listof T))))
(define (rember-f test? a l)
    (match l
        ['() '()]
        [(cons b bs) #:when (test? a b) bs]
        [(cons b bs) (cons b (rember-f test? a bs))]))
(: eq?-c (All (a) (a -> (a -> Boolean))))
(define (eq?-c a)
    (lambda (x) (eq? x a)))
(: multirember&co (All (a b) (a (Listof a) ((Listof a) (Listof a) -> b) -> b)))
(define (multirember&co a lat col)
    (cond
        [(null? lat) (col '() '())]
        [(eq? (car lat) a) 
         (multirember&co 
            a 
            (cdr lat)
            (lambda ([newlat : (Listof a)] [seen : (Listof a)]) (col newlat (cons (car lat) seen))))]
        [else (multirember&co 
            a 
            (cdr lat) 
            (lambda ([newlat : (Listof a)] [seen : (Listof a)]) (col (cons (car lat) newlat) seen)))]))
(: multiinsertLR (All (a) (a a a (Listof a) -> (Listof a))))
(define (multiinsertLR new oldL oldR lat)
    (cond
        [(null? lat) '()]
        [(eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat))))]
        [(eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat))))]
        [else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))]))
(: multiinsertLR&co (All (a b) (a a a (Listof a) ((Listof a) Nonnegative-Integer Nonnegative-Integer -> b) -> b)))
(define (multiinsertLR&co new oldL oldR lat col)
    (cond
        [(null? lat) (col '() 0 0)]
        [(eq? (car lat) oldL) (multiinsertLR&co 
            new 
            oldL 
            oldR 
            (cdr lat) 
            (lambda ([newlat : (Listof a)] [left : Nonnegative-Integer] [right : Nonnegative-Integer]) 
                (col (cons new (cons oldL newlat)) (add1 left) right)))]
        [(eq? (car lat) oldR) (multiinsertLR&co 
            new 
            oldL 
            oldR 
            (cdr lat) 
            (lambda ([newlat : (Listof a)] [left : Nonnegative-Integer] [right : Nonnegative-Integer]) 
                (col (cons oldR (cons new newlat)) left (add1 right))))]
        [else (multiinsertLR&co new oldL oldR (cdr lat) 
            (lambda ([newlat : (Listof a)] [left : Nonnegative-Integer] [right : Nonnegative-Integer]) 
                (col (cons (car lat) newlat) left right)))]))
(define-type (Tree a) (Listof (U a (Tree a))))
(: evens-only* ((Tree Integer) -> (Tree Integer)))
(define (evens-only* tree)
    (cond
        [(null? tree) '()]
        [(and (number? (car tree)) (even? (car tree))) (cons (car tree) (evens-only* (cdr tree)))]
        [(number? (car tree)) (evens-only* (cdr tree))]
        [else (cons (evens-only* (car tree)) (evens-only* (cdr tree)))]))
(: evens-only*&co (All (a) ((Tree Integer) ((Tree Integer) Nonnegative-Integer Nonnegative-Integer -> a) -> a)))
(define (evens-only*&co tree col)
    (cond
        [(null? tree) (col '() 0 0)]
        [(and (number? (car tree)) (even? (car tree))) (evens-only*&co 
            (cdr tree) 
            (lambda ([newtree : (Tree Integer)] [even : Nonnegative-Integer] [odd : Nonnegative-Integer])
                (col (cons (car tree) newtree) (add1 even) odd)))]
        [(number? (car tree)) (evens-only*&co 
            (cdr tree) 
            (lambda ([newtree : (Tree Integer)] [even : Nonnegative-Integer] [odd : Nonnegative-Integer])
                (col newtree even (add1 odd))))]
        [else (evens-only*&co 
            (car tree) 
            (lambda ([newtree : (Tree Integer)] [even : Nonnegative-Integer] [odd : Nonnegative-Integer])
                (evens-only*&co 
                    (cdr tree) 
                    (lambda ([newtree2 : (Tree Integer)] [even2 : Nonnegative-Integer] [odd2 : Nonnegative-Integer])
                        (col (cons newtree newtree2) (+ even even2) (+ odd odd2))))))]))
(: looking (Symbol (Listof (U Integer Symbol)) -> Boolean))
(define (looking a lat)
    (keep-looking a (pick 1 lat) lat))
(: keep-looking (Symbol (U Integer Symbol) (Listof (U Integer Symbol)) -> Boolean))
(define (keep-looking a sorn lat)
    (if (symbol? sorn) 
        (eq? sorn a) 
        (keep-looking a (pick sorn lat) lat)))
(: pick (All (a) (Integer (Listof a) -> a)))
(define (pick n l) (list-ref l n))
(: shift ((List (Listof Any) (Listof Any)) -> Any))
(define (shift x)
    (cons (caar x) (cons (cadr x) (cdr x))))
; (: align ((U Symbol (List (Listof Any) (Listof Any))) -> Any))
; (define (align pora)
;     (cond
;         [(symbol? pora) pora]
;         [(pair? (car pora)) (align (shift pora))]
;         [else (cons (car pora) (align (cdr pora)))]))
(define-type (Entry a) (List (Listof Symbol) (Listof a)))
(define-type (Table a) (Listof (Entry a)))
(: lookup-in-entry (All (a) (Symbol (Entry a) (Symbol -> a) -> a)))
(define (lookup-in-entry name entry fail)
    (match entry
        [(list '() '()) (fail name)]
        [(list '() _) (error 'values-too-long)]
        [(list _ '()) (error 'names-too-long)]
        [(list (cons n ns) (cons v vs)) (if (eq? n name) v (lookup-in-entry name (list ns vs) fail))]))
(: lookup-in-table (All (a) (Symbol (Table a) (Symbol -> a) -> a)))
(define (lookup-in-table name table fail)
    (match table
        ['() (fail name)]
        [(cons entry rest) (lookup-in-entry name entry (lambda ([n : Symbol]) (lookup-in-table n rest fail)))]))
(define-type Expr 
    (U 
        Number 
        Boolean 
        Symbol 
        (List 'quote Expr) 
        (List 'lambda (Listof Expr) Expr)
        (List 'cond (Listof (List Expr Expr)))
        (Listof Expr)))
(define-type Value
    (U
        Number
        Boolean
        Symbol
        (List 'primitive (U 'cons 'car 'cdr 'eq? 'null? 'atom? 'zero? 'add1 'sub1 'number?))
        (List 'non-primitive (List (Table Value) (Listof Symbol) Expr))
        (Listof Value)))
(define-type Action (Expr (Table Value) -> Value))
(: *const Action)
(define (*const e _)
    (cond
        [(number? e) e]
        [(eq? e #t) #t]
        [(eq? e #f) #f]
        [else (list 'primitive e)]))
(: *identifier Action)
(define (*identifier e table)
    (lookup-in-table (assert e symbol?) table (lambda ([_ : Symbol]) (car '()))))
(: *quote Action)
(define (*quote e _)
    (cadr (assert e pair?)))
(: *lambda Action)
(define (*lambda e table)
    (list 'non-primitive (cons table (cdr (assert e pair?)))))
(define table-of car)
(define formals-of cadr)
(define body-of caddr)
(: *cond Action)
(define (*cond e table)
    (evcon (cadr (cast e (List 'cond (Listof (List Expr Expr))))) table))
(: evcon (-> (Listof (List Expr Expr)) (Table Value) Value))
(define (evcon lines table)
    (cond
        [(eq? 'else (question-of (car lines))) (meaning (answer-of (car lines)) table)]
        [(meaning (question-of (car lines)) table) (meaning (answer-of (car lines)) table)]
        [else (evcon (cdr lines) table)]))
(: evlis (-> (Listof Expr) (Table Value) (Listof Value)))
(define (evlis es table)
    (map (lambda ([e : Expr]) (meaning e table)) es))
(define question-of car)
(define answer-of cadr)

(: *application Action)
(define (*application e table)
  (let* ([f (cast (meaning (function-of (cast e (Listof Expr))) table) (U (List 'primitive Symbol) (List 'non-primitive (List (Table Value) (Listof Symbol) (Listof Expr)))))]
         [args (cast (arguments-of (cast e (Listof Expr))) (Listof Expr))])
    (app f (evlis args table))))
(define function-of car)
(define arguments-of cadr)
(: primitive?? ((U (List 'primitive Symbol) (List 'non-primitive (List (Table Value) (Listof Symbol) (Listof Expr)))) -> Boolean : (List 'primitive Symbol)))
(define (primitive?? l) (eq? (car l) 'primitive))
(: non-primitive?? ((U (List 'primitive Symbol) (List 'non-primitive (List (Table Value) (Listof Symbol) (Listof Expr)))) -> Boolean : (List 'non-primitive (List (Table Value) (Listof Symbol) (Listof Expr)))))
(define (non-primitive?? l) (eq? (car l) 'non-primitive))
(: app ((U (List 'primitive Symbol) (List 'non-primitive (List (Table Value) (Listof Symbol) (Listof Expr)))) (Listof Value) -> Value))
(define (app f args)
    (cond
        [(primitive?? f) (apply-primitive (cadr f) args)]
        [(non-primitive?? f) (apply-closure (cadr f) args)]
        [else (error 'crash)]))
(: apply-primitive (Symbol (Listof Value) -> Value))
(define (apply-primitive name args)
    (match name
        ['cons (cons (car args) (assert (cadr args) pair?))]
        ['car (car (assert (car args) pair?))]
        ['cdr (cdr (assert (car args) pair?))]
        ['null? (null? (car args))]
        ['eq? (eq? (car args) (cadr args))]
        ['atom? (let
                    ([x (car args)])
                    (cond
                        [(atom? x) #t]
                        [(null? x) #f]
                        [(eq? (car x) 'primitive) #t]
                        [(eq? (car x) 'non-primitive) #t]
                        [else #f]))]
        ['zero? (zero? (assert (car args) number?))]
        ['add1 (add1 (assert (car args) number?))]
        ['sub1 (sub1 (assert (car args) number?))]
        ['number? (number? (car args))]))
(: apply-closure ((List (Table Value) (Listof Symbol) (Listof Expr)) (Listof Value) -> Value))
(define (apply-closure f args)
    (meaning (body-of f) (extend-table (table-of f) (formals-of f) args)))
(: extend-table ((Table Value) (Listof Symbol) (Listof Value) -> (Table Value)))
(define (extend-table table names values)
    (cons (list names values) table))
(: expression-to-action (Expr -> Action))
(define (expression-to-action e)
    (cond
        ((atom? e) (atom-to-action e))
        (else (list-to-action e))))
(: atom-to-action (Expr -> Action))
(define (atom-to-action e)
    (cond
        [(number? e) *const]
        [(eq? e #t) *const]
        [(eq? e #f) *const]
        [(eq? e 'cons) *const]
        [(eq? e 'car) *const]
        [(eq? e 'cdr) *const]
        [(eq? e 'null?) *const]
        [(eq? e 'eq?) *const]
        [(eq? e 'atom?) *const]
        [(eq? e 'zero?) *const]
        [(eq? e 'add1) *const]
        [(eq? e 'sub1) *const]
        [(eq? e 'number) *const]
        [else *identifier]))
(: list-to-action (Expr -> Action))
(define (list-to-action e)
    (cond
        [(atom? e) (error 'bad-dispatch-in-expression-to-action)]
        [(atom? (car e))
            (cond
                [(eq? (car e) 'quote) *quote]
                [(eq? (car e) 'lambda) *lambda]
                [(eq? (car e) 'cond) *cond]
                [else *application])]
        [else *application]))
(: evaluate (Expr -> Value))
(define (evaluate e)
    (meaning e '()))
(: meaning (Expr (Table Value) -> Value))
(define (meaning e table)
    ((expression-to-action e) e table))
