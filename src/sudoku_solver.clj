(ns sudoku-solver)

(def valid-values #{1 2 3 4 5 6 7 8 9})
(def board-size (count valid-values))

(def test-board
  [[5 3 nil nil 7 nil nil nil nil]
   [6 nil nil 1 9 5 nil nil nil]
   [nil 9 8 nil nil nil nil 6 nil]
   [8 nil nil nil 6 nil nil nil 3]
   [4 nil nil 8 nil 3 nil nil 1]
   [7 nil nil nil 2 nil nil nil 6]
   [nil 6 nil nil nil nil 2 8 nil]
   [nil nil nil 4 1 9 nil nil 5]
   [nil nil nil nil 8 nil nil 7 9]])

(defn horizontal-slice
  "This function takes a board and a row index and returns a set of all the known
  values in that row of the board."
  [board row]
  (->> (nth board row)
       (remove nil?)
       (set)))

(defn vertical-slice
  "Like `horizontal-slice` but returns a set of known values based on a given
  column index."
  [board column]
  (->> (mapv #(nth % column) board)
       (remove nil?)
       (set)))

(defn block-slice
  "Given a board, row, and column, determine the sub-grid that this location is
  part of, and return the set of values contained within that inner matrix."
  [board row column]
  (let [row-offset (* 3 (int (/ row 3)))
        col-offset (* 3 (int (/ column 3)))]
    (->> (drop row-offset board)
         (take 3)
         (mapcat #(->> (drop col-offset %)
                       (take 3)))
         (remove nil?)
         (set))))

(defn location-set
  "Given a board, row, and column, return a set of values actively utilized by
  the defined position on the board. This should describe all of the values that
  we *can't* use to fill in a given location, should it be blank."
  [board row column]
  (clojure.set/union
    (horizontal-slice board row)
    (vertical-slice board column)
    (block-slice board row column)))

(defn location-missing-set
  "Iff the location on the board defined by the row and column is nil, return
  the difference of the used set of values for this location from the full set
  of possible values. This would describe the potential values that could fill
  a blank spot."
  [board row column]
  (when (nil? (get-in board [row column]))
    (clojure.set/difference valid-values (location-set board row column))))

(defn suggested-replacements
  "Given a board, identify all of the possible (unvetted,) possible values for
  each location on the board that doesn't have a value already."
  [board]
  (->> (for [x (range board-size)
             y (range board-size)]
         (when (nil? (get-in board [x y]))
           [[x y] (location-missing-set board x y)]))
       (remove nil?)))

(defn certain-replacements
  "Given a collection of suggested moves on the board, find the ones where there
  is only one possible value and keep those, while also pulling it out of the
  set on the second position of the 2-tuple in the collection."
  [suggested]
  (->> (filter #(= (count (second %)) 1) suggested)
       (map #(update % 1 first))))

(defn replace-one
  "Given a board and collection of changes we can make, take a single change and
  apply it to the board and return the board."
  [board change-coll]
  (let [[[row column] value] (first change-coll)]
    (assoc-in board [row column] value)))

(defn solve
  "Given a board, solve it."
  [board]
  (let [complete? (-> (mapcat identity board)
                      (set)
                      (contains? nil)
                      (not))]
    (if complete?
      board
      (let [suggested-replacements (suggested-replacements board)
            certain-replacements (certain-replacements suggested-replacements)]
        (recur (replace-one board certain-replacements))))))

(comment

  (def $board-state (atom test-board))

  (swap! $board-state #(replace-one % (certain-replacements (suggested-replacements %))))

  (block-slice @$board-state 2 4)

  (certain-replacements (suggested-replacements @$board-state))

  (certain-replacements (suggested-replacements (replace-many test-board (certain-replacements (suggested-replacements test-board)))))
  
  (time (solve test-board))

  (get-in test-board [4 0])

  )