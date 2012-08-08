(ns ^{:doc "Wrapper for guava math classes and methods"
      :author "Adam Wang<wizzardcloud@gmail.com>"}
  clj.guava.math
  (:import [com.google.common.math BigIntegerMath DoubleMath IntMath LongMath]
           [java.math BigInteger RoundingMode]))

(def rounding-mode {
                    :up RoundingMode/UP
                    :down RoundingMode/DOWN
                    :ceiling RoundingMode/CEILING
                    :floor RoundingMode/FLOOR
                    :half-up RoundingMode/HALF_UP
                    :half-down RoundingMode/HALF_DOWN
                    :half-even RoundingMode/HALF_EVEN
                    :unnecessary RoundingMode/UNNECESSARY})

(defn- to-biginteger
  "Convert clojure number to java.math.BigInteger"
  [n]
  (BigInteger/valueOf n))

(defn binomial
  "Returns n choose k, also known as the binomial coefficient of n and k, that is, n! / (k! (n - k)!)."
  {:added "0.1" :static true}
  [n k & L]
  (if (= L :L)
    (LongMath/binomial n k)
    (let [tp1 (type n) tp2 (type k)]
      (cond
       (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/binomial n k)
       :default (BigIntegerMath/binomial n k)
       )))
)

(defn divide
  " Returns the result of dividing p by q, rounding using the specified RoundingMode."
  { :added "0.1" :static true}
  [p q ^RoundingMode mode & L]
 (cond
  (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
  :default (if (= L :L)
             (LongMath/divide p q (mode rounding-mode))
             (let [tp1 (type p) tp2 (type q)]
                 (cond
                  (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/divide p q (mode rounding-mode))
                  :default (BigIntegerMath/divide (to-biginteger p) (to-biginteger q) (mode rounding-mode))
                  )))
 ))

(defn factorial
  "Returns n!, that is, the product of the first n positive integers, or 1 if n == 0."
  {:added "0.1" :static true}
  [n & L]
  (cond 
    (= L :L) (LongMath/factorial n)
    (= L :D) (DoubleMath/factorial n)
    :default (let [tp (type n)]
      (cond
       (= tp java.lang.Integer) (IntMath/factorial n)
       :default (BigIntegerMath/factorial n)
       )))
)

(defn is-power-of-two
  "Returns true if x represents a power of two."
  {:added "0.1" :static true}
  [x & L]
  (if (= L :L)
    (LongMath/isPowerOfTwo x)
    (let [tp (type x)]
      (cond
       (= tp java.lang.Long) (BigIntegerMath/isPowerOfTwo (to-biginteger x))
       (= tp java.lang.Double) (DoubleMath/isPowerOfTwo x)
       (= tp java.lang.Integer) (IntMath/isPowerOfTwo x)
       )))
)


(defn log10
  "Returns the base-10 logarithm of x, rounded according to the specified rounding mode."
  {:added "0.1" :static true}
  [x ^RoundingMode mode & L]
  (if (= L :L)
    (LongMath/log10 x (mode rounding-mode))
    (cond
     (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
     :default  (let [tp (type x)]
                 (cond
                  (= tp java.lang.Integer) (IntMath/log10 x (mode rounding-mode))
                  :default (BigIntegerMath/log10 (to-biginteger x) (mode rounding-mode))
                  ))
     ))
)

(defn log2
  "Returns the base-2 logarithm of x, rounded according to the specified rounding mode. The rounding mode is optional"
  { :added "0.1" :static true}
  ([^double x] (DoubleMath/log2 x))
  ([x ^RoundingMode mode & L]
  (cond
   (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
   :default (if (= L :L)
              (LongMath/log2 x (mode rounding-mode))
              (let [tp (type x)]
                (cond
                 (= tp java.lang.Double) (DoubleMath/log2 x (mode rounding-mode))
                 (= tp java.lang.Integer) (IntMath/log2 x (mode rounding-mode))
                 :default (BigIntegerMath/log2 (to-biginteger x) (mode rounding-mode))
                 )
                ))
   ))
)

(defn sqrt
    "Returns the square root of x, rounded with the specified rounding mode."
    {:added "0.1" :static true}
    [x ^RoundingMode mode & L]
    (cond
     (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
     :default (if (= L :L)
                (LongMath/sqrt x (mode rounding-mode))
                (let [tp (type x)]
                    (cond
                     (= tp java.lang.Integer) (IntMath/sqrt x (mode rounding-mode))
                     :default (BigIntegerMath/sqrt (to-biginteger x) (mode rounding-mode)))
                    ))
     )
)

(defn round-to-int
    "Returns the int value that is equal to x rounded with the specified rounding mode, if possible."
    { :tag DoubleMath :added "0.1" :static true}
    [^double x ^RoundingMode mode]
    (cond
     (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
     :default (DoubleMath/roundToInt x (mode rounding-mode)))
)

(defn round-to-long
    "Returns the long value that is equal to x rounded with the specified rounding mode, if possible."
    { :tag DoubleMath :added "0.1" :static true}
    [^double x ^RoundingMode mode]
    (cond
     (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
     :default (DoubleMath/roundToLong x (mode rounding-mode)))
)
  
(defn round-to-biginteger
  "Returns the BigInteger value that is equal to x rounded with the specified rounding mode, if possible."
  { :tag DoubleMath :added "0.1" :static true}
  [^double x ^RoundingMode mode]
  (cond
   (not (mode rounding-mode)) (throw (IllegalArgumentException. "No such rounding mode. Please see rounding-mode"))
   :default (DoubleMath/roundToBigInteger x (mode rounding-mode)))
)

(defn is-mathematical-integer
  "Returns true if x represents a mathematical integer."
  { :tag boolean :added "0.1" :static true}
  [^double x]
  (DoubleMath/isMathematicalInteger x)
)

(defn pow
  "Returns b to the kth power. Even if the result overflows, it will be equal to BigInteger.valueOf(b).pow(k).intValue(). This implementation runs in O(log k) time."
  {:added "0.1" :static true}
  [b k]
  (let [tp (type b)]
    (cond
     (= tp java.lang.Long) (LongMath/pow b k)
     :default (IntMath/pow b k)))
)

(defn mod
  "Returns x mod m. This differs from x % m in that it always returns a non-negative result."
  {:added "0.1" :static true}
  [x m]
  (let [tp1 (type x) tp2 (type m)]
    (cond
     (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/mod x m)
     (= tp1 java.lang.Long) (LongMath/mod x m)))
)

(defn gcd
  "Returns the greatest common divisor of a, b. Returns 0 if a == 0 && b == 0."
  {:added "0.1" :static true}
  [a b]
  (let [tp1 (type a) tp2 (type b)]
    (cond
     (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/gcd a b)
     :default (LongMath/gcd a b)))
)

(defn checked-add
  "Returns the sum of a and b, provided it does not overflow."
  {:added "0.1" :static true}
  [a b]
  (let [tp1 (type a) tp2 (type b)]
    (cond
     (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/checkedAdd a b)
     :default (LongMath/checkedAdd a b)))
)

(defn checked-sub
  "Returns the difference of a and b, provided it does not overflow."
  {:added "0.1" :static true}
  [a b]
  (let [tp1 (type a) tp2 (type b)]
    (cond
     (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/checkedSubtract a b)
     :default (LongMath/checkedSubtract a b)))
)

(defn checked-multiply
  "Returns the product of a and b, provided it does not overflow."
  {:added "0.1" :static true}
  [a b]
  (let [tp1 (type a) tp2 (type b)]
    (cond
     (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/checkedMultiply a b)
     :default (LongMath/checkedMultiply a b)))
)

(defn checked-pow
  "Returns the b to the kth power, provided it does not overflow."
  {:added "0.1" :static true}
  [b k]
  (let [tp1 (type b) tp2 (type k)]
    (cond
     (and (= tp1 java.lang.Integer) (= tp2 java.lang.Integer)) (IntMath/pow b k)
     :default (LongMath/pow b k)))
)

