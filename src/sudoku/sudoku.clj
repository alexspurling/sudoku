(ns sudoku.sudoku
  (:require clojure.set))

(def puzzle
  [[0 0 3  0 2 0  6 0 0]
   [9 0 0  3 0 5  0 0 1]
   [0 0 1  8 0 6  4 0 0]

   [0 0 8  1 0 2  9 0 0]
   [7 0 0  0 0 0  0 0 8]
   [0 0 6  7 0 8  2 0 0]

   [0 0 2  6 0 9  5 0 0]
   [8 0 0  2 0 3  0 0 9]
   [0 0 5  0 1 0  3 0 0]])

(def one-to-nine (set (range 1 10)))

(defn get-row [p y]
  (nth p y))

(defn get-col [p x]
  (map #(nth % x) p))

(defn get-square [p x y]
  (let [square-x (* 3 (quot x 3))
        square-y (* 3 (quot y 3))
        row1 (subvec (get-row p square-y) square-x (+ 3 square-x))
        row2 (subvec (get-row p (+ 1 square-y)) square-x (+ 3 square-x))
        row3 (subvec (get-row p (+ 2 square-y)) square-x (+ 3 square-x))]
    (concat row1 row2 row3)))

(defn- get-valid-values-at [p [y x]]
  (let [used-values (concat (get-row p y)
                            (get-col p x)
                            (get-square p x y))]
    (clojure.set/difference one-to-nine (set used-values))))

(defn- cell-solutions [p c]
  "Given a puzzle, returns a set of puzzles with valid values
   at cell [x y]"
  (if (zero? (get-in p c))
    (map #(assoc-in p c %) (get-valid-values-at p c))
    [p]))

(defn solve
  ([p]
    (solve p (for [x (range 9) y (range 9)] [x y])))
  ([p [c & coords]]
    (if c
      (let [solutions (cell-solutions p c)]
        (first (keep #(solve % coords) solutions)))
      p))) ;If there are no more coords to check then just return what we've got