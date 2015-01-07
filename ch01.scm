
(define (cube x)
  (* x x x))

;;; Exercise 1.11

;; A function f is defined such that f(n) = n if n < 3 and f(n) =
;; f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3.


(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))


(define (f n)
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 3))))

(define (f-iter a b c count)
  (let ((f-of-this (+ (* 3 a) (* 2 b) c)))
    (if (= count 0)
        f-of-this
        (f-iter b c f-of-this (- count 1)))))

;; It works:

(equal? (f-recur 17) (f 17))

; #t

;;; Exercise 1.12

;; Compute a given element in Pascal's Triangle by means of a recursive process.



;;; Exercise 1.16

;; Define an iterative recursive process to calculate the result of an
;; exponentiation in logarithmic time.

;; Hint: use a state variable a, and define state transformation such
;; that (* a (expt b n)) remains constant through each transformation.

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 1) (* b a))
        ((even? n) (expt-iter (* b b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b)))))

;;; Exercise 1.21

;; Use the smallest-divisor procedure to find the smallest divisors
;; for 199, 1999, and 19999.

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? divisor n)
  (= (remainder divisor n) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; Exercise 1.22

;; Provided timed-prime-test procedure:

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n) (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Write a procedure search-for-primes that checks the primality of
;; consecutive odd integers in a specified range.

(define (search-for-primes floor ceiling)
  (if (even? floor)
      (search-for-primes (+ floor 1) ceiling)
      (if (< floor ceiling)
          ((timed-prime-test floor)
           (search-for-primes (+ floor 2) ceiling)))))

;; Nonfunctional placeholder for MIT Scheme's "runtime" construct
;; until I can look up a replacement for Gambit scheme:

(define (runtime)
  100)

;;; Exercise 1.23

;; Define procedure next such that it returns 3 if input 2 and n + 2
;; for any other input n. Modify smallest-divisor to use this. Does
;; this run twice as fast? If not, why?

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

;; (Remember to un-modify find-divisor to use (+ test-divisor 1)
;; instead of (next test-divisor))


;;; Exercise 1.24

;; Modify timed-prime-test to use fast-prime? and test the same 12
;; primes from the last exercise.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer n))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (fast-search-for-primes floor ceiling)
  (cond ((even? floor)
         (fast-search-for-primes (+ floor 1) ceiling))
        ((< floor ceiling)
         (timed-fast-prime? floor)
         (fast-search-for-primes (+ floor 2) ceiling))
        (else (newline))))

(define (timed-fast-prime? n)
  (newline)
  (display n)
  (start-fast-prime-test n (runtime)))

(define (start-fast-prime-test n start-time)
  (if (fast-prime? n 10) (report-prime (- (runtime) start-time))))

(define (runtime)
  (time->seconds (current-time)))


;;;; 1.3 Formulating Abstractions with Higher-Order Procedures

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (inc x)
  (+ x 1))

;;; Exercise 1.29

;; Define an integral-computing procedure using Simpson's Rule.

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n))
           (y k))
          ((even? k) (* (y k) 2))
          (else (* (y k) 4))))
  (* (/ h 3) (sum term 0 inc n)))

;;; Exercise 1.30

;; Rewrite sum to execute iteratively. (filling in template)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result a))))
  (iter a 0))

;;; Exercise 1.31

;; Define a function product, iteratively and recursively, which
;; returns the product of the values of a given function over a given
;; range. 

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result a))))
  (iter a 1))

;;; Exercise 1.32

;; Show that both product and sum are cases of a more general
;; accumulate function, recursively and iteratively.

(define (accumulate combiner null-value term a next b)
  (define (acc term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (acc term (next a) next b))))
  (acc term a next b))

;;; Exercise 1.33

;; Filtered accumulate (iteratively, since I didn't write the
;; iterative accumulate)

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (filtered-acc term a next b)
    (define (iter a result)
      (if (> a b)
          result
          (if (filter a)
              (iter (next a) (combiner result a))
              (iter (next a) result))))
    (iter a null-value))
  (filtered-acc term a next b))

;; Sum of squares of primes in range a to b: (Does not work because
;; "prime?" is not implemented.)

(define (sum-of-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

;;; Exercise 1.34

;; Given:

(define (f g)
  (g 2))

;; What happens if we ask the interpreter (f f)?

;; ]=> (f f)
;; ; Error: 2 is not an operator.

;;; Exercise 1.35

;; Given:

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Show that the Golden Ratio is a fixed point of the transformation x
;; -> 1 + 1/x using fixed-point.

;; Theta is defined as the number where theta squared equals theta
;; plus one. Divide by x to arrive at the above transformation.

(define (transformation x)
  (+ 1 (/ 1 x)))

(fixed-point transformation 1.0) ;]=> 1.6180327...


;;; Exercise 1.36

;; Modify fixed-point so it prints the approximations it generates.


(define (verbose-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Then find a solution to x^x = 100 with and without average damping.

;; Undamped:

(verbose-fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

;; Damped (converges much faster):

(define (average x y)
  (/ (+ x y) 2))

(verbose-fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)

;;; Exercise 1.37

;; Infinite continued fractions, recursively then iteratively.

(define (cont-frac n d k)
  (if (= k 0)
      (/ (n k) (+ (d k)
                  (/ (n k) (d k))))
      (/ (n k) (+ (d k)
                  (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (cont-frac i acc)
    (if (= i 0)
        acc
        (cont-frac (- i 1) (/ (n i) (+ (d i) acc)))))
  (cont-frac (- k 1) (/ (n k) (d k))))

;;; Exercise 1.38

;; Approximate e based on Euler's expansion of (e-2) as a continuous
;; fraction.

(define (euler-d x)
  (if (= (remainder (+ x 1) 3) 0)
      (expt 2 (+ 1 (/ (- x 2) 3)))))

(cont-frac-iter (lambda (x) 1.0) euler-d 100)

;; Rewritten sqrt

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)
                                1.0))))

;; Newton's Method

(define dx 0.0000001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;; Exercise 1.40

;; cubic

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;; Exercise 1.41

;; double

(define (double f)
  (lambda (x)
    (f (f x))))

;;; Exercise 1.42 - compose

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; Exercise 1.43 - nth application of f

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;;; Exercise 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f (+ x dx))
          (f x)
          (f (- x dx)))
       3)))
