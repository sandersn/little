#lang racket
(define var vector)
(define var? vector?)
(define lhs car)
(define rhs cdr)
(define empty-s '())
(define (ext-s x v s)
  (cons (cons x v) s))
(define size-s length)
(define-syntax run
    (syntax-rules ()
        ((_ n^ (x) g ...)
         (let ((n n^) (x (var 'x)))
           (if (or (not n) (> n 0))
             (map-inf n (lambda (s) (reify (walk* x s))) ((all g ...) empty-s))
             '())))))
(define (walk* v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) v)
      ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
      (else v))))
(define (walk v s)
  (cond
    ((var? v) (cond
                ((assq v s) => (lambda (a) (walk (rhs a) s)))
                (else v)))
    (else v)))
(define (reify v)
  (walk* v (reify-s v empty-s)))
(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v) (ext-s v (reify-name (size-s s)) s))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))
(define (reify-name n)
  (string->symbol (string-append "-" "." (number->string n))))
(define-syntax case-inf
  (syntax-rules ()
    ((_ e on-zero ((a^) on-one) ((a f) on-choice))
     (let ((a-inf e))
       (cond
         ((not a-inf) on-zero)
         ((not (and (pair? a-inf) (procedure? (cdr a-inf))))
          (let ((a^ a-inf))
            on-one))
         (else (let ((a (car a-inf)) (f (cdr a-inf)))
                 on-choice)))))))
(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))
(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))
(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))
(define (map-inf n p a-inf)
    (case-inf a-inf
      '()
      ((a) (cons (p a) '()))
      ((a f) (cons (p a)
                (cond
                  ((not n) (map-inf n p (f)))
                  ((> n 1) (map-inf (sub1 n) p (f)))
                  (else '()))))))
(define succeed (lambda (s) (unit s)))
(define fail (lambda (s) (mzero)))
(define (== v w)
  (lambda (s)
    (cond
      ((unify v w s) => succeed)
      (else (fail s)))))
(define (unify v w s)
  (let ((v (walk v s)) (w (walk w s)))
    (cond
      ((eq? v w) s)
      ((var? v) (ext-s v w s))
      ((var? w) (ext-s w v s))
      ((and (pair? v) (pair? w))
       (cond
         ((unify (car v) (car w)) => (lambda (s) (unify (cdr v) (cdr w) s)))
         (else #f)))
      ((equal? v w) s)
      (else #f))))
(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g ...)
     (lambda (s)
       (let ((x (var 'x)) ...)
         ((all g ...) s))))))
;; TODO: One of conde, cond-aux or ife is transcribed incorrectly
(define-syntax conde
  (syntax-rules ()
    ((_ c ...) (cond-aux ife c ...))))
(define-syntax cond-aux
  (syntax-rules (else)
    ((_ ifer) fail)
    ((_ ifer (else g ...)) (all g ...))
    ((_ ifer (g ...)) (all g ...))
    ((_ ifer (g0 g ...) c ...)
     (ifer g0
       (all g ...)
       (cond-aux ifer c ...)))))
(define-syntax ife
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambda (s)
       (mplus ((all g0 g1) s) (lambda () (g2 s)))))))
(define-syntax all
    (syntax-rules ()
        ((_ g ...) (all-aux bind g ...))))
(define-syntax all-aux
    (syntax-rules ()
      ((_ bnd) succeed)
      ((_ bnd g) g)
      ((_ bnd g0 g ...)
       (let ((g^ g0))
         (lambda (s)
           (bnd (g^ s) (lambda (s) ((all-aux bnd g ...) s))))))))
(define (mplus a-inf f)
  (case-inf a-inf
    (f)
    ((a) (choice a f))
    ((a f0) (choice a (lambda () (mplus (f0) f))))))
(define (bind a-inf g)
  (case-inf a-inf
    (mzero)
    ((a) (g a))
    ((a f) (mplus (g a) (lambda () (bind (f) g))))))
(run #f (q) fail)