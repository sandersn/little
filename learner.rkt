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
(refr '(1 2 3 4) 2)

(define (line x)
    (lambda (theta) (+ (* (ref theta 0) x) (ref theta 1))))
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
      ; TODO: sqr and - need to be lifted to tensor application
        (sum-1 (sqr (- ys pred-ys)))))))