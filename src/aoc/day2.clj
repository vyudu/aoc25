(use 'aoc.core)

(def input (map #(str/split % #"-") (str/split (str/trim-newline (slurp "resources/input")) #",")))

(defn chunk-string
  [n s]
  (let [len (count s)
        chunk-len (quot len n)
        rem-len (rem len n)]
    (if (= 0 rem-len)
      (map #(apply str %) (partition-all chunk-len s))
      (let [power (- (* n (+ 1 chunk-len)) 1)]
        (chunk-string n (str (reduce * (repeat power 10)))
)))))

(defn rpt-string-as-int [n s] (parse-long (apply str (repeat n s))))

(defn in-range?
  [sense rptblock chunks]
  (let [first-different (first (drop-while #(= % rptblock) chunks))]
    (if (nil? first-different)
      true
      (sense first-different rptblock))))

(defn internal-rpts? [n] (if (= 1 (count n)) true (not (boolean (re-matches #"(\d+)\1+" n)))))

(defn sum-invalid-ids
  ([num-rpts double-count? input]
    (reduce + (map #(apply (partial sum-invalid-ids num-rpts double-count?) %) input)))
  ([num-rpts double-count? lo hi]
    (let [[lo1 & lochunks] (map parse-long (chunk-string num-rpts lo))
          [hi1 & hichunks] (map parse-long (chunk-string num-rpts hi))
          cands (if (in-range? <= lo1 lochunks)
                  (if (in-range? >= hi1 hichunks)
                      (range lo1 (+ hi1 1))
                      (range lo1 hi1))
                  (if (in-range? >= hi1 hichunks)
                      (range (+ lo1 1) (+ hi1 1))
                      (range (+ lo1 1) hi1)))
          pred (if double-count? identity internal-rpts?)]
      (reduce + (map (partial rpt-string-as-int num-rpts) (filter pred (map str cands))))
)))

(defn solve
  [input]
  {:part1 (sum-invalid-ids 2 true input)
   :part2 (reduce + (map #(sum-invalid-ids % false input) (range 2 (apply max (map (comp count first) input)))))})

(solve input)
