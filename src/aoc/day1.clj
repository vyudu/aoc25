(use 'aoc.core)

(defn locker-sum
  [a b]
  (let [[dir amt] (str/split b #"(?=\d)" 2)]
    (if (= "L" dir)
      (- a (read-string amt))
      (+ a (read-string amt)))))

(defn zero-crosses
  [a b]
  (let [[s1 s2] (map #(Math/floorDiv % 100) [a b])
        diff (abs (- s2 s1))]
     (if (= 0 (mod b 100))
       (if (or (= 0 (mod a 100)) (> s2 s1)) diff (+ diff 1))
       (if (and (= 0 (mod a 100)) (< s2 s1)) (- diff 1) diff))))

(defn solve
 [input]
 (let [cumsums (reductions locker-sum 50 input)]
    {:part1 (count (filter #(= 0 (mod % 100)) cumsums))
     :part2 (reduce + (map #(apply zero-crosses %) (partition 2 1 cumsums)))}
    ))

(solve input)
