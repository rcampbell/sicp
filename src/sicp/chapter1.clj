(ns sicp.chapter1
  (:use clojure.test))

;;;; Chapter 1 - Building Abstractions with Procedures
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-9.html#%_chap_1

;;;; Section 1.1 - The Elements of Programming
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-10.html#%_sec_1.1

;;; Exercise 1.1

(is (= 10  10))
(is (= 12  (+ 5 3 4)))
(is (=  8  (- 9 1)))
(is (=  3  (/ 6 2)))
(is (=  6  (+ (* 2 4) (- 4 6))))
(is (= (def a 3) (var a))) ; Scheme returns nil, while
                           ; Clojure returns the newly
                           ; created var. Also, since
                           ; evaluation is from left to
                           ; right, the def must be to the
                           ; left of the literal var.
(is (= (def b (+ a 1)) (var b)))
(is (=    19  (+ a b (* a b))))
(is (= false  (= a b))) ; Clojure uses true/false rather than
                            ; Scheme's #t/#f
(is (=     4  (if (and (> b a) (< b (* a b)))
                b
                a)))
(is (=    16  (cond (= a 4) 6 ; Clojure's cond doesn't require the
                              ; predicate+consequent expression
                              ; clauses to be wrapped in parens
                              ; like Scheme does.
                    (= b 4) (+ 6 7 a)
                    :else 25))) ; Clojure uses an :else keyword
                                ; rather than the else symbol
                                ; found in Scheme
(is (=     6  (+ 2 (if (> b a) b a))))
(is (=    16  (* (cond (> a b) a
                       (< a b) b
                       :else -1) 
                 (+ a 1))))


;;; Exercise 1.2

(is (= -37/150
       (/ (+ 5
             4
             (- 2
                (- 3
                   (+ 6 4/5))))
          (* 3
             (- 6 2)
             (- 2 7)))))


;;; Exercise 1.3

(defn- square [x] (* x x))

(defn- sum-of-squares [x y]
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

;; Mine is overly complex. Nuno's version is better:
;; https://github.com/nfma/sicp/blob/master/section1.1/ex1.3.txt
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

(def tolerance 0.001)

(defn- delta [x y]
  (Math/abs (- x y)))

(defn- within-tolerance? [x y]
  (< (delta x y)
     tolerance))

(defn- average [x y]
  (/ (+ x y) 2))

(defn- ^:dynamic improve [guess x]
  (average guess (/ x guess)))

(defn- ^:dynamic good-enough? [guess x]
  (within-tolerance? (square guess) x))

(defn ^:dynamic sqrt
  ([x]
     (sqrt 1.0 x))
  ([guess x]
     (if (good-enough? guess x)
       guess
       (recur (improve guess x)
              x))))

(is (within-tolerance? (sqrt Integer/MAX_VALUE)
                       (Math/sqrt Integer/MAX_VALUE)))

;; Show how it's broken for smaller values...
(let [small-num 0.0001]
  (is (not (within-tolerance? (sqrt small-num)
                              (Math/sqrt small-num))))) ; sqrt completes,
                                                        ; but the result
                                                        ; is outside our
                                                        ; tolerance

;; Show how it's broken for larger values...
(binding [improve (fn [old-guess x]
                    (let [new-guess (average old-guess (/ x old-guess))]
                      (if (< (delta x new-guess)
                             (delta x old-guess))
                        new-guess
                        (throw (RuntimeException.
                                "New guess is worse!")))))]
  (is (thrown-with-msg? RuntimeException #"New guess is worse!"
        (sqrt Long/MAX_VALUE))))

;; Now fix it
(binding [good-enough? (fn [old-guess new-guess]
                         (< (/ (delta old-guess
                                      new-guess)
                               old-guess)
                            0.00001)) ; very small percentage change
          sqrt         (fn ([x]
                             (sqrt 0.1 1.0 x))
                          ([old-guess new-guess x]
                             (if (good-enough? old-guess new-guess)
                               new-guess
                               (recur new-guess
                                      (improve new-guess x)
                                      x))))]
  (let [small-num 0.0001]
    (is (within-tolerance? (sqrt small-num)
                           (Math/sqrt small-num))))
  (let [large-num Long/MAX_VALUE]
    (is (within-tolerance? (sqrt large-num)
                           (Math/sqrt large-num)))))


;;; Excercise 1.8

(letfn [(cube [x] (* x x x))        
        (improve [guess x] (/ (+ (/ x
                                    (square guess))
                                 (* 2 guess))
                              3))
        (good-enough? [guess x] (within-tolerance? (cube guess) x))]
  
  (defn cbrt
    ([x]
       (cbrt 1.0 x))
    ([guess x]
       (if (good-enough? guess x)
         guess
         (recur (improve guess x)
                x))))

  (is (within-tolerance? (cbrt Integer/MAX_VALUE)
                         (Math/cbrt Integer/MAX_VALUE))))


;;;; Section 1.2 - Procedures and the Processes They Generate
;;;; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_sec_1.2


