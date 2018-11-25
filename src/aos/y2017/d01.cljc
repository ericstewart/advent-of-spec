(ns aos.y2017.d01
  (:require
   [aos.y2017.input :refer [input-d01] :rename {input-d01 input}]
   [aos.utils :as u]
   [clojure.test :refer [deftest is testing]]
   [clojure.string :as str]))

(def answer-p1 995)
(def answer-p2 1130)

(deftest solution-ea5d7bf3 []
  (let [p1 (reduce
            +
            (map
             (fn [a b]
               (if (= a b)
                 (u/parse-int (str a)) 0))
             input
             (drop 1 (cycle input))))
        half (/ (count input) 2)
        p2 (reduce
            +
            (map
             (fn [a b]
               (if (= a b)
                 (u/parse-int (str a)) 0))
             input
             (drop half (cycle input))))]
    (is (= answer-p1 p1))
    (is (= answer-p2 p2))))

(deftest solution-56af0364
  (letfn [(str->digits [s]
            (map (zipmap "0123456789" (range)) s))]
    (let [data (-> input str/trim str->digits)]
      (letfn [(matches [xs ys]
                (->>
                 (map vector xs ys)
                 (filter (partial apply =))
                 (map first)))
              (solve [pair-up]
                (apply + (matches data (pair-up data))))]
        (let [p1 (solve #(rest (cycle %)))
              p2 (solve #(nthrest (cycle %) (/ (count %) 2)))]
          (is (= answer-p1 p1))
          (is (= answer-p2 p2)))))))

(deftest solution-d61416be
  (letfn [(char-code [char]
            #?(:clj (int char)
               :cljs (.charCodeAt char 0)))
          (cs->nums [cs]
            (map #(- (char-code %) (char-code \0)) cs))
          (part-1 []
            (let [cs input
                  ns (cs->nums cs)
                  c (first ns)]
              (->> (concat ns [c])
                   (partition 2 1)
                   (filter (fn [[x y]] (= x y)))
                   (map first)
                   (apply +))))
          (part-2 []
            (let [cs input
                  len (count cs)
                  ns (cycle (cs->nums cs))]
              (->> (map list ns (drop (quot len 2) ns))
                   (take len)
                   (filter (fn [[x y]] (= x y)))
                   (map first)
                   (apply +))))]
    (let [p1 (part-1)
          p2 (part-2)]
      (is (= answer-p1 p1))
      (is (= answer-p2 p2)))))

(deftest solution-aa6460bf
  (letfn [(inverse-captcha [pairs]
            (transduce
             (comp (filter (partial apply =))
                   (map first)
                   (map #?(:clj int, :cljs #(.charCodeAt % 0)))
                   (map #(- % 48)))
             +
             pairs))
          (part-1 [s]
            (let [pairs (->> (take (inc (count s)) (cycle s))
                             (partition 2 1))]
              (inverse-captcha pairs)))
          (part-2 [s]
            (let [pairs (->> (split-at (/ (count s) 2) s)
                             cycle
                             (partition 2 1)
                             (take 2)
                             (mapcat (partial apply map vector)))]
              (inverse-captcha pairs)))]
    (let [p1 (part-1 input)
          p2 (part-2 input)]
      (is (= answer-p1 p1))
      (is (= answer-p2 p2)))))
