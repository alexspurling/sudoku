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

(def puzzle2
  [[5 3 4 6 7 8 9 1 2]
   [6 7 2 1 9 5 3 4 8]
   [1 9 8 3 4 2 5 6 7]

   [8 5 9 7 6 1 4 2 3]
   [4 2 6 8 5 3 7 9 1]
   [7 1 3 9 2 4 8 5 6]

   [9 6 1 5 3 7 2 8 4]
   [2 8 7 4 1 9 6 3 5]
   [3 4 5 2 8 6 1 7 0]])

; There are no duplicate digits (other than 0)
(defn is-valid-set [s]
  (let [no-zeros (filter (complement zero?) s)]
    (if (empty? no-zeros)
      true
      (apply distinct? no-zeros))))

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

(defn get-all-sets [p]
  (let [square-coords (for [x (range 3) y (range 3)] [(* 3 x) (* 3 y)])
        all-squares (map (fn [[sx sy]] (get-square p sx sy)) square-coords)
        all-rows (map (partial get-row p) (range 9))
        all-cols (map (partial get-col p) (range 9))]
    (concat all-squares all-rows all-cols)))

(defn is-valid
  ([p]
   "Returns true if every set of 9 cells is valid."
    (every? is-valid-set (get-all-sets p)))
  ([p [x y]]
    "Returns true if all the rows, columns and squares that
    intersect with x, y are valid"
    (let [sets [(get-row p y) (get-col p y) (get-square p x y)]]
      (every? true? (map is-valid-set sets)))))

(defn is-solved [p]
  "A puzzle is solved if every number is not zero"
  (and (every? #(every? (complement zero?) %) p)
       (is-valid p)))

(defn- cell-solutions [p [x y]]
  "Given a puzzle, returns a set of puzzles with valid values
   at cell x y"
  (if (zero? (get-in p [x y]))
    (let [solutions (map #(assoc-in p [x y] %) (range 1 10))]
      (filter #(is-valid % [x y]) solutions))
    [p]))

(defn solve [p]
  (reduce (fn [puzzles cell]
            (println (str "Looking at " (count puzzles) " solutions at " cell))
            (mapcat #(cell-solutions % cell) puzzles))
          [p]
          (for [x (range 9) y (range 9)] [x y])))
