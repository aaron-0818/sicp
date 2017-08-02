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


(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (display " *** ")
	(display elapsed-time))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))
        ))
  (newline)
  (display n)
  (start-prime-test n (runtime))
)
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

(define (smallest-prime-after number)
  (if (prime? number)
      number
      (smallest-prime-after (+ 1 number))))


(define (timed-smallest-prime-after number)
   (define (report-time-and-prime number start-time)
     (display (smallest-prime-after number))
     (display " using time ")
     (display (- (runtime) start-time)) )
   (newline)
   (display number)
   (display " next number is ")
   (report-time-and-prime number (runtime)))