(ns sudoku.sudoku)

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

(defn- is-valid-placement [p [y x] v]
  "Placement of value v is valid at position x y if that value
  does not already exist in any row, column or square"
  (cond
    (some #{v} (get-row p y)) false
    (some #{v} (get-col p x)) false
    (some #{v} (get-square p x y)) false
    :else true))

(defn- cell-solutions [p c]
  "Given a puzzle, returns a set of puzzles with valid values
   at cell [x y]"
  (if (zero? (get-in p c))
    (let [solutions (filter #(is-valid-placement p c %) (range 1 10))]
      (map #(assoc-in p c %) solutions))
    [p]))

(defn solve
  ([p]
    (solve p (for [x (range 9) y (range 9)] [x y])))
  ([p [c & coords]]
    (if c
      (let [solutions (cell-solutions p c)]
        (first (keep #(solve % coords) solutions)))
      p))) ;If there are no more coords to check then just return what we've got