(ns dlazdice.core)

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(Math/abs b) 0 1]
        (zero? b) [(Math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (Math/abs b)
                     r0 (Math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn merge-solutions ; merges solutions with form k + a*x and l + b*y into m + c*z (where a,b,c are any integers >= 0)
  [[k x] [l y]]
  (let [[pair-gcd a b] (extended-gcd x y) ;; pair-gcd = a*x + b*y
        solution-exists? (->> (Math/abs (- k l))
                              (gcd pair-gcd)
                              (= (Math/abs pair-gcd)))]
    (if solution-exists?
      (let [m (let [diff (- k l)
                    x' (/ (lcm x y) x) ;; (lcm x y) = x'*x = y'*y
                    a' (Math/abs (* a (/ diff pair-gcd))) ;; diff = a'*x + b'*y
                    a'' (if (< 0 (* diff a)) (- x' a') a') 
                    a''' (mod a'' x') 
                    new-diff (+ k (* a''' x))]
                (if (zero? diff)
                  k
                  new-diff))
            z (lcm x y)]
        [m z])
      (throw (Exception. "No solution")))))

(defn solutions
  [args]
  (->> (map (fn [[a b]] (map (fn [x] (int (* 10 x))) [a b])) args)
       (map (fn [[a b]] [b (+ a b)]))       
       (reduce merge-solutions)))

(defn solution
  [[min-x min-y] xs]
  (let [[a b] (solutions xs)] ;; solutions = a + l*b (for l = 0, 1, 2...)
    (map #(/ (+ a (* (Math/ceil (/ (- (* 10 %) a) b)) b)) 10) [min-x min-y])))

(comment
  (solution [200 300] [[24 0] [18 0]]) ;=> (216.0 360.0)
  (solution [200 50000.5] [[7.0 0.5]
                           [16.0 0.3]
                           [10.8 0.2]]) ;=> No solution
  (solution [10 1000000] [[24.6 0.2]
                          [15.8 0.2]
                          [16.2 0.4]]) ;=> (28768.2 1016800.2)
  )