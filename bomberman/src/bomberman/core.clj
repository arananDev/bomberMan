(ns bomberman.core
  (:gen-class)
  (:require [clojure.string :as str]))



(def grid
  ".......
...O...
....O..
.......
OO.....
OO.....")




(defn grid-to-array
  [grid]
  (loop [grid-split (str/split-lines grid)
         data []]
    (if (= 0 (count grid-split))
      data
      (recur (rest grid-split) (conj data (str/split (first grid-split) #""))))))



(defn get-initial-bomb-locations
  [grid]
  (let [r (count grid)
        c (count (get-in grid[0]))]
    (for [i (range r)
          j (range c)
          :when (= "O" (get-in grid [i j]))]
      [i j])
    ))





(defn get-bomb-explotation
  [grid index]
  (let [r (count grid)
        c (count (get-in grid[0]))
        i (first index)
        j (second index)
        results (atom [[i j]])]
    (if (> i 0) (swap! results conj [(dec i) j]) nil)
    (if (> j 0) (swap! results conj [i (dec j)]) nil)
    (if (< i (dec r)) (swap! results conj [(inc i) j]) nil)
    (if (< j (dec c)) (swap! results conj [i (inc j)]) nil)
    @results
    )
  )


(defn aftermath
  [grid second-bombs]
  (loop [results #{}
         indices second-bombs]
    (if (= 0 (count indices))
      results 
      (let [explosion (get-bomb-explotation grid (first indices))]
        (recur (into results explosion) (rest indices))
        ))
    ))



(defn check-grid
  [index aftermath]
  (if (contains? aftermath index)
    "."
    "O"))



(defn full-cycle
  [grid]
  (let [r (count grid)
        c (count (get-in grid [0]))
        new-grid (atom grid)
        aftermath (aftermath grid (get-initial-bomb-locations grid))]
    (doseq [i (range r)
            j (range c)]
      (reset! new-grid (assoc-in @new-grid [i j] (check-grid [i j] aftermath))))
    @new-grid
    ))


(defn arr-to-str
  [arr]
  (loop [result ""
         grid arr]
    (if (= 1 (count grid))
      (str result (apply str (first grid)))
      (recur (str result (apply str (first grid)) "\n")
             (rest grid)))))




(defn bomberMan [n grid]
  (if (even? n)
    (str/replace grid #"." "O")
    (loop [grid-arr (grid-to-array grid)
           i 1]
      (if (= i n)
        (arr-to-str grid-arr)
        (let [next-i (+ 2 i)]
          (if (= 0 (mod next-i 3))
            (recur (full-cycle grid-arr) next-i)
            (recur grid-arr next-i))))) 
   ))

