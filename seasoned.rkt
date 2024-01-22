#lang typed/racket
(: intersectall : (All (a) (Listof (Listof a)) -> (Listof a)))
(define (intersectall lset)
    (call/cc (lambda ([hop : ((Listof a) -> (Listof a))])
        (letrec ([A : ((Listof (Listof a)) -> (Listof a)) (lambda (lset)
                        (cond
                            [(null? (car lset)) (hop '())]
                            [(null? (cdr lset)) (car lset)]
                            [else (I (car lset) (A (cdr lset)))]))]
                 [I : ((Listof a) (Listof a) -> (Listof a)) (lambda (s1 s2)
                    (letrec ([J : ((Listof a) -> (Listof a)) (lambda (s1)
                                    (cond
                                        [(null? s1) '()]
                                        [(memq (car s1) s2) (cons (car s1) (J (cdr s1)))]
                                        [else (J (cdr s1))]))])
                        (J s1)))])
            (if (null? lset) '() (A lset))))))
(define-type (Kons a d) ((a d -> (U a d)) -> (U a d)))
(: kons (All (a d) (a d -> (Kons a d))))
(define (kons kar kdr)
    (lambda (selector)
        (selector kar kdr)))
(: kar (All (a d) ((Kons a d) -> (U a d))))
(define (kar k)
    (k (lambda (a _) a)))
(: kdr (All (a d) ((Kons a d) -> (U a d))))
(define (kdr k)
    (k (lambda (_ d) d)))
; (: bons (All (a d) (a -> (((d -> Void) a d -> (U a d)) -> (U a d)))))
; (define (bons kar)
;     (let (([kdr : d] (ann '()  d)))
;         (lambda (selector)
;             (selector (lambda ([x : d]) (set! kdr x)) kar kdr))))
(: example (-> Any)) ; the return type is, I think, infinite, because
                     ; the continuation is expected to return the return type of the type annotation
                     ; which in turn is expected to return the return type of the type annotation,
                     ; forever.
                     ; So I fell back to any.
(define (example)
    (call/cc (lambda ([k : (Integer -> Integer)]) ; (also means this return type is wrong)
        k)))
; good luck calling this continuation!
(: leave Any)
(define leave (call/cc (lambda ([k : (Integer -> Any)]) k)))