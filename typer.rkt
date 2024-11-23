#lang pie
(claim jumbo Nat)
(define jumbo 42)
(claim length (Pi ((T U) (xs (List T))) Nat))
(define length
  (lambda (T xs)
    (rec-List xs
      0
      (lambda (x xs length-1)
        (add1 length-1)))))

; (car 'spinach)
