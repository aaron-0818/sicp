(define (cube x)
  (* x x x))

(define (is-sum? operation)
  (if (= 11 (operation 5 6))
      True
      False))
(define (is-multi operation)
  (if (= 30 (operation 5 6))
      True
      False))

(define (give-base operation)
  (cond ((is-sum? operation) 0)
        ((is-multi? operation) 1)))
(define (integral-basic f a b dx)
  (define (add-dx number)
    (+ number dx))
  (* (sum f add-dx (+ a (/ dx 2)) b) dx))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (simpson-next x)
    (+ x (* 2 h)))
  (define (simpson-term y)
    (+ (* 4 (f y)) (* 2 (f (+ y h)))))
  (* (/ h 3) (+ a (- b) (sum simpson-term simpson-next (+ a h) b))))

(define (filtered-accumulator-recur filter operation term next a b)
  (if (> a b)
      (give-base operation)
      (if (filer a)
          (operation (term a) (accumulator-recur operation term next (next a) b))
          (accumulator-recur filter operation term next (next a) b))
      ))

(define (filtered-accumulator-iter filter operation term next a b)
  (define (iter  x result)
    (if (> x b)
        result
        (if (filter x)
            (iter (next x) (operation result (term x))))
            (iter (next x) result)))
(iter a (give-base operation)))