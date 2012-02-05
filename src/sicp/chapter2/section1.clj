(ns chatper2.section1
  (:refer-clojure :exclude (cons))
  (:use clojure.test))

;; Exercise 2.1

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (rem a b))))

(defn make-rat [n d]
  (let [g (gcd n d)
        n (/ n g)
        d (/ d g)]
    (if (and (pos? n) (neg? d))
      [(* n -1) (* d -1)]
      [n d])))


;; Exercise 2.2

(def make-segment vector)

(def start-segment first)

(def end-segment second)

(def make-point vector)

(def x-point first)

(def y-point second)

(defn midpoint-segment [[[x1 y1] [x2 y2]]]
  [(/ (+ x1 x2) 2)
   (/ (+ y1 y2) 2)])


;; Exercise 2.3

(def make-rect vector)

(letfn [(width  [[[x1 _] [x2 _]]]
                (Math/abs (- x1 x2)))
        (height [[[_ y1] [_ y2]]]
                (Math/abs (- y1 y2)))]
  
  (defn rect-perimeter [hypotenuse]
    (* 2 (+ (width  hypotenuse)
            (height hypotenuse))))

  (defn rect-area [hypotenuse]
    (* (width  hypotenuse)
       (height hypotenuse))))


;; Exercise 2.4

(defn cons [x y]
  (fn [m]
    (m x y)))

(defn car [z]
  (z (fn [p _]
       p)))

(defn cdr [z]
  (z (fn [_ q]
        q)))

