(define (smallest-divisor n)
	(define (divides? a b)
	  (= (remainder b a) 0))
  (define (find-divisor n current-divisor)
    (cond ((> (square current-divisor) n) n)
          ((divides? current-divisor n) current-divisor)
          (else (find-divisor n (+ current-divisor 1)))))
(find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))


;;fermet's little theorem: If n is a prime number and a is 
;;any positive integer less than n, then a raised to the nth 
;;power is congruent to a modulo n.

(define (expmod base exponent m)
  (cond ((= 0 exponent) 1)
        ((even? exponent) (remainder (square 
        (expmod base (/ exponent 2) m) m)))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))



(define (fast-prime?  n times)
;;Random returns a nonnegative integer less than its integer input
(define (fermet-test n)
  (define (try-it a)
    (= a (expmod a n n)))
 (try-it (+ 1 (random (- n 1)))))
  (cond ((= times 0) true)
        ((fermet-test n) (fast-prime? n (- times 1)))
        (else false)))