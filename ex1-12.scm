(define (pascal row colunm)
  (if (or (= 1 colunm) (= row colunm))
      1
      (+ (pascal (- row 1) (- colunm 1)) (pascal (- row 1) colunm))))

