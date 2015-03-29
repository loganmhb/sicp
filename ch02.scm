;;;; Chapter 02

;; Rational number system basis

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

;; Printing utility

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Basic addition, subtraction of rats

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer y) (denom x))
     (* (numer x) (denom y))))

;; 

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Exercise 2.1

;; Improve make-rat to handle negative arguments by normalizing the
;; sign.

(define (sgn x)
  (if (= x 0)
      0
      (/ (abs x) x)))

(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (cons (/ (* (sgn d) n) g) (/ (abs d) g))))

;;; Exercise 2.2

;; Points and midpoints

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p q)
  (cons p q))

(define (start-point segment)
  (car segment))

(define (end-point segment)
  (cdr segment))

(define (midpoint segment)
  (make-point (average (x-point (start-point segment))
                       (x-point (end-point segment)))
              (average (y-point (start-point segment))
                       (y-point (end-point segment)))))


;;; Exercise 2.4

;; Alternate cons, car, cdr

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; Define cdr.

(define (cdr z)
  (z (lambda (p q) q)))

;;; Exercise 2.5

;; Verify that we can represent pairs of integers a, b using only
;; numbers and arithmetic operations if we represent a, b as 2^a *
;; 3^b.

(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (car z)
  (let ((loop (lambda (z acc)
                (if (= (remainder z 3) 0)
                    (loop (/ z 3) acc)
                    (loop (/ z 2) (+ acc 1))))))
    (loop z 0)))

(define (cdr z)
  (let ((loop (lambda (z acc)
                (if (= (remainder z 2) 0)
                    (loop (/ z 2) acc)
                    (loop (/ z 3) (+ acc 1))))))
    (loop z 0)))


;;; Exercise 2.6 - church numerals

(define zero
  (lambda (f) (lambda (x) x)))

(define (add-one n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; Substitution model for direct representation of one:

;; One:

(define one (lambda (f) (lambda (x) (f x))))

;; Two:

(define two (lambda (f) (lambda (x) (f (f x)))))

;; Addition: (does not work)

(define (church-add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define (church-print x)
  ((x inc) 0))

(define (inc x)
  (+ x 1))


(lambda (f) (lambda (x) (f (f x))))


;;; Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (upper-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (spans-zero? y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      true
      false))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Divisor spans zero.")
      (mul-interval x
                    (make-interval (/ 1.0 (lower-bound y))
                                   (/ 1.0 (upper-bound y))))))

(define (make-interval a b)
  (cons a b))

;; Exercise 2.7 - complete Alyssa P. Hacker's implementation

(define (upper-bound x)
  (car x))

(define (lower-bound x)
  (cdr x))

;; Exercise 2.8 - define sub-interval

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


;;; Exercise 2.17 - last-pair

(define (last-pair l)
  (if (eq? (cdr l) '())
      (car l)
      (last-pair (cdr l))))

;;; Exercise 2.18 - reverse

(define (reverse list)
  (define (acc result list)
    (if (null? list)
        result
        (acc (cons (car list) result) (cdr list))))
  (acc (car list) (cdr list)))

;;; Exercise 2.19 - count change revisited

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (eq? coin-values '()))

;; Exercise 2.20 - same-parity

(define (same-parity? n . args)
  (define (iter n args acc)
    (cond ((eq? (cdr args) '())
           (if (eq? (odd? n) (odd? (car args))) (append acc (list (car args))) acc))
          ((eq? (odd? n) (odd? (car args)))
           (iter n (cdr args) (append acc (list (car args)))))
          (else
           (iter n (cdr args) acc))))
  (iter n args '()))

;; Exercise 2.22 - square-list

;; Why does this produce a reversed list?

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; Here we're popping numbers off the front of list items (because
;; that's how car and cdr allow us to access the numbers) then
;; building a new list with those answers from the back (because
;; that's how cons works).

;; Why doesn't switching the arguments to cons help?

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; Now the answers are in the right order but the cons cells are
;; backwards -- we're consing a list onto an item, not an item onto a
;; list.

;; Exercise 2.23 - for-each

;; for-each takes a proc and a list of items, and applies the proc to
;; each item in sequence. It does not do anything with the values
;; producde by applying the proc to the items (should be used for e.g.
;; printing).

(define (for-each proc items)
  (if (eq? items '())
      #t
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;; Exercise 2.27 - deep-reverse

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

(define (deep-reverse items)
  (cond ((null? items)
         items)
        ((pair? (car items))
         (append (deep-reverse (cdr items))
                 (list (deep-reverse (car items)))))
        (else
         (append (deep-reverse (cdr items))
                 (list (car items))))))

;; Exercise 2.28 - fringe (flatten)

(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(define (fringe tree)
  (let recur ((x tree)
              (acc '()))
    (cond ((null? x)
           acc)
          ((not (pair? x))
           (cons x acc))
          (else (recur (car x) (recur (cdr x) acc))))))

;;; Exercise 2.29 - binary mobile

;; Constructors

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; Accessors

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (mobile? structure)
        (total-weight structure)
        structure)))

(define (mobile? structure)
  (pair? structure))

;; Functions

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced-branch? branch)
  (if (mobile? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))

(define (balanced? mobile)
  (if (and (eq? (torque (left-branch mobile))
                (torque (right-branch mobile)))
           (and (balanced-branch? (left-branch mobile))
                (balanced-branch? (right-branch mobile))))
      #t
      #f))


;;; Exercise 2.30 - square-tree

;; Test case:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;; => (1 (4 (9 16) 25) (36 49))

(define (square x)
  (* x x))

;; Old-fashioned:

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;; With map and recursion:

(define (square-tree tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (square subtree)
             (square-tree subtree)))
       tree))

;;; Exercise 2.31 - tree-map (abstracting from prev. exercise)

(define (tree-map f tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (f subtree)
             (tree-map f subtree)))
       tree))

;; Test case:

(tree-map square '(1 (2 (3 4) 5) (6 7)))

;;; Exercise 2.32 - Subsets

;; Complete the given definition. Why does it work?

(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))
                          rest)))))

;; Test case:

(subsets '(1 2 3))

;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; The procedure proceeds recursively to the end of the set, the empty list. It then
;; successively works backwards, adding to the list of subsets the results of adding
;; the new element to each previously-found subset (including the empty list, giving
;; a set of the new element alone), thereby reaching every possible combination of
;; subsets.


;;;; Sequences as Conventional Interfaces

;; New functions per signal-flow diagrams

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;; Exercise 2.33

;; Reformulate list manipulations as accumulations.

(define (map p sequence)
  (accumulate (lambda (item initial) (cons (p item) initial))
              '()
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x count) (if (not (null? x)) (+ count 1) count))
              0
              sequence))

;;; Exercise 2.34

;; Write a procedure that evaluates polynomials using Horner's Rule.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x)
                   this-coeff))
              0
              coefficient-sequence))

;;; Exercise 2.35

;; Redefine count-leaves as an accumulation.

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

;;; Exercise 2.36 - accumulate-n

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; Exercise 2.37 - matrix algebra

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; returns vector t where t(i) = sum to j of m(i)(j)*v(j)

(define (matrix-*-vector m v)
  (map (lambda (mi)
         (accumulate + 0 (accumulate-n * 1 (list mi v))))
       m))

(define (transpose mat)
  (accumulate-n list '() mat))

;; returns matrix p where p(i)(j) = sum to k of m(i)(k)*n(k)(j)

;; p[i][j] = sum to k of m[i][k]*n[k][j]

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;;; Exercise 2.38

;; Provided:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; Commutative ops will produce the same result whether folded right or left.

;;; Exercise 2.39

(define nil '())

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;;; Exercise 2.40 - Unique pairs

;; Given an integer n, generate the sequence of unique pairs i, j such that 1<j<i<n.

(define (enumerate-interval x y)
  (if (> x y)
      nil
      (cons x (enumerate-interval (+ x 1) y))))

(define (flatmap op seq)
  (accumulate append nil (map op seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;;; Exercise 2.41

;; Write a procedure to find all ordered triples of distinct positive integers i,
;; j, and k less than or equal to a given integer n that sum to a given integer s.

(define (ordered-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (find-triplet-addends n s)
  (filter (lambda (triple) (eq? (fold-left + 0 triple) s))
          (ordered-triples n)))

;;; Exercise 2.42 - n queens

;; Queen safety:
;; -No queen in the same column is guaranteed
;; -No queen must have the same row as the queen in col k
;; -No queen must have the same (row + col) as the queen in col k
;; -No queen must satisfy (qr - qkr) = (qc - qkc)

(define (make-position r c)
  (cons r c))

(define (row queen)
  (car queen))

(define (col queen)
  (cdr queen))

(define (adjoin-position new-row k rest-of-queens)
  (cons (make-position new-row k) rest-of-queens))

(define empty-board '())

(define (safe? k positions)
  (let ((queenk (car positions))    ; newest position is always in car
        (others (cdr positions)))
    (define (attacks? pos1 pos2)
      (or (eq? (row pos1) (row pos2))
          (eq? (+ (row pos1) (col pos1))
               (+ (row pos2) (col pos2)))
          (eq? (- (col pos1) (col pos2))
               (- (row pos1) (row pos2)))))
    (eq? (filter (lambda (pos) (attacks? pos queenk))
                 others)
         '())))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;;;;; A PICTURE LANGUAGE

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;;; Exercise 2.44

;; Define the procedure up-split used by corner-split. It is similar to
;; right-split, except that it switches the roles of below and beside.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;; Exercise 2.45

;; Define a general splitting operation in order to reimplement
;; right-split and up-split as follows:

;;   (define right-split (split beside below))
;;   (define up-split (split below beside))

(define (split large-op small-op)
  (letrec ((func (lambda (painter n)
                   (if (= n 0)
                       (painter)
                       (let ((smaller (func painter (- n 1))))
                         (large-op painter (small-op smaller smaller)))))))
    func))

;;;; Frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;;; Exercise 2.46

;; Implement a data abstraction for vectors running from the origin to
;; a point, comprising a constructor make-vect and selectors xcor-vect
;; and ycor-vect. In terms of this abstraction, implement add-vect,
;; sub-vect, and scale-vect.

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

;-----------

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1)
                (xcor-vect vect2))
             (+ (ycor-vect vect1)
                (ycor-vect vec2))))

(define (sub-vect vect1 vec2)
  (make-vect (- (xcor-vect vect1)
                (xcor-vect vect2))
             (- (ycor-vect vect1)
                (ycor-vect vec2))))

(define (scale-vect vect scalar)
  (make-vect (* (xcor-vect scalar))
             (* (ycor-vect scalar))))

;;; Exercise 2.47

;; Two constructors for frames. For each, supply the appropriate
;; selectors.

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;; selectors

(define (frame-origin frame)
  (car frame))
(define (frame-edge1 frame)
  (cadr frame))
(define (frame-edge2 frame)
  (caddr frame))

;---

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;; selectors

(define (frame-origin frame)
  (car frame))
(define (frame-edge1 frame)
  (cadr frame))
(define (frame-edge2 frame)
  (cddr frame))

;;; Exercise 2.48

;; A directed line segment in a plane can be represented by two
;; vectors, each running from the origin to one endpoint. Define
;; constructor and selectors.

(define (make-segment vector1 vector2)
  (cons vector1 vector2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;;; Skipping several easy exercises on quotation

