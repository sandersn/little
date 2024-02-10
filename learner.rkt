#lang racket
; alternatively, install MALT fromm the little learner web site
(define tensor vector)
(define tensor? vector?)
; TODO: Should only be: Real Number
(define scalar? number?)
(define ref list-ref)
(define tref vector-ref)
(define tlen vector-length)
(define len length)
(define (gradient-of f theta)
; this bit was guessed by copilot! Don't use it!
  (λ (x)
    (/ (- (f (+ x 0.0001) theta) (f x theta)) 0.0001)))
(define (dot-product t u)
; same, except it would be right if t and u weren't vectors
  (apply + (map * t u)))
(define (trefs t b)
  (list->vector (map (lambda (i) (ref t i)) b)))
(define refr drop)

(define (tensor-lift f)
  (define (tensor-apply x y)
    (cond
      ((and (scalar? x) (scalar? y)) (f x y))
      ((scalar? x) (vector-map (lambda (y) (tensor-apply x y)) y))
      ((scalar? y) (vector-map (lambda (x) (tensor-apply x y)) x))
      ((and (tensor? x) (tensor? y)) (vector-map tensor-apply x y))))
  tensor-apply)
(define (tensor-lift1 f)
  (define (tensor-apply x)
    (if (scalar? x)
      (f x)
      (vector-map tensor-apply x)))
  tensor-apply)
(define (tensor-lift2 f)
  (define (tensor-apply x)
    (cond
      ((scalar? x) x) ; but is actually not supposed to happen
      ((scalar? (tref x 0)) (f x))
      (else (vector-map tensor-apply x))))
  tensor-apply)
(define +-* (tensor-lift +))
(define --* (tensor-lift -))
(define *-* (tensor-lift *))
(define sqr-* (tensor-lift1 sqr))
(define sum (tensor-lift2 sum-1))

(define (line x)
    (lambda (theta) (+-* (*-* (ref theta 0) x) (ref theta 1))))
(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define (rank tensor)
  (let loop ((acc 0) (t tensor))
    (if (scalar? t)
      acc
      (loop (add1 acc) (tref t 0)))))
(define (shape tensor)
    (let loop ((acc '()) (t tensor))
        (if (scalar? t)
            acc
            (loop (cons (tlen t) acc) (tref t 0)))))

(define (sum-1 tensor)
  (let loop ((i (sub1 (tlen tensor))) (acc 0))
    (if (zero? i)
      (+ acc (tref tensor 0))
      (loop (sub1 i) (+ acc (tref tensor i))))))
(define (l2-loss target)
  (lambda (xs ys)
    (lambda (theta)
      (let ((pred-ys ((target xs) theta)))
        (sum-1 (sqr-* (--* ys pred-ys)))))))

(define (revise f revs theta)
  (cond
    ((zero? revs) theta)
    (else (revise f (sub1 revs) (f theta)))))