;;for small numbers like 0.000001, the difference is small due to
;;that the number is itself small, so the precision is meaningless
;;in absolute sense. for large numbers, even for good answer, 
;;the difference may still be big due to the number is itself large
;;result in long run time or endless loop;

;; new implementation of good-enough?
((define (new-good-enough? guess x)
  (< (/ (abs (- (improve guess x) guess)) guess) 0.001)))