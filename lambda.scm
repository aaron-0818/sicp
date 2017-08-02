;;(lambda (<formal-parameter>) 	<body>)

;;(let ((<var1> <exp1>)
;;	  (<var2> <exp2>)
;;	  (<var3> <exp3>))
;;	(body))

;;Let allows one to bind variables as locally as possible to where they are to be used. 
;;The variables’ values are computed outside the let. 

;;Ex1.34: The object 2 is not applicable.
(define (average a b)
  (/ (+ a b) 2))


(define (negtive? t)
  (< t 0))
(define (search f neg-point pos-point)
	(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

  (let ((midpoint (average neg-point pos-point)))
  	(if (close-enough? neg-point pos-point)
  	    midpoint
  	    (let ((test-value (f midpoint)))
  	    	(cond ((positive? test-value) 
  	    	(search f neg-point midpoint))
  	    	      ((negtive? test-value) 
  	    	      (search f midpoint pos-point)) 
  	    	  (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
  	(cond ((and (negtive? a-value) (positive? b-value)) (search f a b))
  	      ((and (negtive? b-value) (positive? a-value)) (search f b a))
  	      (else (error "values are not of oppsite sign" a b)))))


;;fixed point
(define tolerence 0.0001)
(define (fixed-point f init-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerence))
(define (try guess)
  (display guess)
  	(newline)
  (let ((next (f guess)))
  	
  	(if (close-enough? guess next)
  	    next
  	    (try next)))
(try init-guess)))

(define (print a)
  (display a)
  (newline)
  (display a)
  (newline))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)