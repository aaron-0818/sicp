(define (f-recur n)
  (cond ((< n 3) n)
        (else (+ (f-recur (- n 1))
        		(* 2 (f-recur (- n 2)))
        		(* 3 (f-recur (- n 3)))))))

(define (f n)
	(define (f-process a b c)
	  (+ a (* 2 b) (* 3 c)))
	(define (find-base x)
	  (if (< x 3)
	      x
	      (find-base (- x 1))))
	
	(define base
	  (find-base n))
	(define (f-iter a b c count)
	  (if (= count n)
	      a
	      (f-iter (f-process a b c) a b (+ count 1))))
	(f-iter base (- base 1) (- base 2) base))	