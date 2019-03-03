(define (average x y)
  (/ (+ x y) 2))

(define (cube-root-function guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (square-root-function guess x)
  (/ x guess))

(define (improve guess x f)
  (average guess (f guess x)))

(define (good-enough? guess x f)
  (< (abs (- guess (improve guess x f))) (* (/ 1 100000000000000000) guess)))                                                                                                                                    

(define (general-iter guess x f)
  (if (good-enough? guess x f)
      guess
      (general-iter (improve guess x f)
		    x
		    f)))

(define (my-sqrt x)
  (general-iter 1.0 x square-root-function))

(define (my-cuberoot x)
  (general-iter 1.0 x cube-root-function))

;(display (my-sqrt 10))
(display (my-cuberoot 10))
