(ns sicp.chapter1)

;;;; Chapter 1

;;; Exercise 1.1
(assert (= 10  10))
(assert (= 12  (+ 5 3 4)))
(assert (=  8  (- 9 1)))
(assert (=  3  (/ 6 2)))
(assert (=  6  (+ (* 2 4) (- 4 6))))
(assert (= (def a 3) (var a))) ; Scheme returns nil, while
                               ; Clojure returns the newly
                               ; created var. Also, since
                               ; evaluation is from left to
                               ; right, the def must be to the
                               ; left of the literal var.
(assert (= (def b (+ a 1)) (var b)))
(assert (=    19  (+ a b (* a b))))
(assert (= false  (= a b))) ; Clojure uses true/false rather than
                            ; Scheme's #t/#f
(assert (=     4  (if (and (> b a) (< b (* a b)))
                    b
                    a)))
(assert (=    16  (cond (= a 4) 6 ; Clojure's cond doesn't require the
                                  ; predicate+consequent expression
                                  ; clauses to be wrapped in parens
                                  ; like Scheme does.
                        (= b 4) (+ 6 7 a)
                        :else 25))) ; Clojure uses an :else keyword
                                    ; rather than the else symbol
                                    ; found in Scheme
(assert (=     6  (+ 2 (if (> b a) b a))))
(assert (=    16 (* (cond (> a b) a
                          (< a b) b
                          :else -1) 
                    (+ a 1))))

;;; Exercise 1.2
(assert (= -37/150
           (/ (+ 5
                 4
                 (- 2
                    (- 3
                       (+ 6 4/5))))
              (* 3
                 (- 6 2)
                 (- 2 7)))))

;;; Exercise 1.3
(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x)
     (square y)))

(defn largest-sum-of-squares [a b c]
  "Using only conds"
  (cond (>= a b) (cond (>= b c) (sum-of-squares a b)
                       :else    (sum-of-squares a c))
        (>= b c) (cond (>= c a) (sum-of-squares b c)
                       :else    (sum-of-squares b a))
        (>= c a) (cond (>= a b) (sum-of-squares c a)
                       :else    (sum-of-squares c b))))

(defn largest-sum-of-squares' [a b c]
  "Using higher-order fns"
  (let [larger ((comp (partial take 2) reverse sort) [a b c])]
    (apply + (map * larger larger))))

;;; Exercise 1.4
;;; The if conditional selects which operator to use at runtime based
;;; on the value of b.

;;; Exercise 1.5
;;; If Ben uses an applicative-order interpreter the expression
;;; (test 0 (p)) will evaluate forever, recursively evaluating the
;;; operand (p) to no end. If Ben is uses a normal-order interpreter,
;;; the evaluation will return 0 since (p) will never have to be
;;; evaluated due to the short-circuited if conditional.

;;; Excercise 1.6
;;; Using new-if the sqrt-iter procedure would recurse forever because
;;; new-if is not a special form with special operand evaluation
;;; rules. In other words, new-if will eagerly evaluate its operands -
;;; in this case a recursive call to sqrt-iter - rather than using
;;; lazy evaluation like the primative if special form.

;;; Excercise 1.7
(letfn [(average [x y] (/ (+ x y) 2))
        (improve [guess x] (average guess (/ x guess)))
        (good-enough? [guess x] (< (Math/abs (- (square guess) x))
                                   0.001))]
  (defn sqrt
    ([x]
       (sqrt 1.0 x))
    ([guess x]
       (if (good-enough? guess x)
         guess
         (recur (improve guess x)
                x)))))

(square (sqrt 0.0001)) ; 0.001... fail

;; TODO, fix good-enough per execise

;;; Excercise 1.8
(letfn [(cube [x] (* x x x))        
        (improve [guess x] (/ (+ (/ x
                                    (square guess))
                                 (* 2 guess))
                              3))
        (good-enough? [guess x] (< (Math/abs (- (cube guess) x))
                                   0.001))]
  (defn cube-root
    ([x]
       (cube-root 1.0 x))
    ([guess x]
       (if (good-enough? guess x)
         guess
         (recur (improve guess x)
                x)))))
