(ns aos.y2017.d02
  (:require
   [aos.utils :as u]
   [aos.y2017.input :refer [input-d02] :rename {input-d02 input}]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(def answer-p1 44887)
(def answer-p2 242)

(deftest solution-2887c5bd
  (letfn [(part-1 []
            (transduce
             (comp
              (map #(str/split % #"\s"))
              (map #(map u/parse-int
                         %))
              (map (fn [row]
                     [(apply max row)
                      (apply min row)]))
              (map (fn [[max min]]
                     (- max min))))
             +
             (str/split-lines input)))
          (find-divisibles [nums]
            (let [desc (sort-by - nums)
                  asc  (sort nums)]
              (for [greater desc
                    smaller asc
                    :while (> greater smaller)
                    :when (zero? (mod greater smaller))]
                [greater smaller])))
          (part-2 []
            (transduce
             (comp
              (map #(str/split % #"\s"))
              (map #(map u/parse-int
                         %))
              (map (fn [row]
                     (first (find-divisibles row))))
              (map (fn [[greater smaller]]
                     (/ greater smaller))))
             +
             (str/split-lines input)))]
    (let [p1 (part-1)
          p2 (part-2)]
      (is (= answer-p1 p1))
      (is (= answer-p2 p2)))))

(deftest solution-ce255f30
  (let [data (->> [input]
                  (map #(str/split % #"\t"))
                  (map #(map u/read-string %)))]
    (letfn [(solve [f]
              (transduce
               (map f)
               +
               data))
            (part-1 []
              (solve #(- (apply max %) (apply min %))))
            (divides? [x y]
              (and (not= 0 x y)
                   (zero? (mod y x))))

            (dividing-pairs [xs]
              (for [x1 xs
                    x2 xs
                    :when (and (distinct? x1 x2)
                               (divides? x1 x2))]
                [x1 x2]))
            (first-integer-ratio [xs]
              (when-let [[x y] (first (dividing-pairs xs))]
                (/ y x)))
            (part-2 []
              (solve first-integer-ratio))]
      (let [p1 (part-1)
            p2 (part-2)]
        (is (number? p1))
        (is (number? p2))))))

(deftest solution-34fb1eea
  (letfn [(line-nums [line]
            (u/read-string (str "[" line "]")))
          (solve1 [lines]
            (letfn [(line-diff [line]
                      (->> (line-nums line)
                           ((juxt #(apply max %) #(apply min %)))
                           (apply -)))]
              (transduce (map line-diff) + 0 lines)))
          (part-1 []
            (solve1 [input]))
          (solve2 [lines]
            (letfn [(line-div [line]
                      (first
                       (for [[x & ys] (->> (line-nums line)
                                           (iterate rest)
                                           (take-while seq))
                             y ys
                             :when (or (zero? (mod x y))
                                       (zero? (mod y x)))]
                         (/ (max x y) (min x y)))))]
              (transduce (map line-div) + 0 lines)))
          (part-2 []
            (solve2 [input]))]
    (let [p1 (part-1)
          p2 (part-2)]
      (is (number? p1))
      (is (number? p2)))))
