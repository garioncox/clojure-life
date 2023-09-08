;; Game of life rules
;; 1) A living cell with 2 > neighbors > 3 dies
;; 2) A dead cell with 3 neighbors comes to life
;; 3) Any other cell remains in its current state

(ns demo)

;; (range 8) gives the values for [i]
(defn neighbors-of [[x y]]
  (let [dx [-1 0 1 -1 1 -1 0 1]
        dy [-1 -1 -1 0 0 1 1 1]]
    (map (fn [i] [(+ x (dx i)) (+ y (dy i))]) 
         (range 8))))

(defn living-neighbors
  ([cell living]
   (let [x (first cell), y (second cell)]
     (count (filter living (neighbors-of [x y])))))
  ([living]
   (fn [cell] (living-neighbors cell living))))

(defn will-live? [[x y] living]
  (let [neighbors (living-neighbors [x y] living)]
    (if (or
         (and (living [x y]) (< 1 neighbors) (> 4 neighbors))
         (and (not= (living [x y]) true) (= 3 neighbors)))
      true false)))

(defn next-generation [living]
  (filter (fn [cell] (will-live? cell living)) (distinct (apply concat (map neighbors-of living)))))

(defn board-to-string [living]
  (let [min-x (apply min-key (fn [[x y]] (+ x 0)) living)
        max-x (apply max-key (fn [[x y]] (+ x 0)) living)
        min-y (apply min-key (fn [[x y]] (+ y 0)) living)
        max-y (apply max-key (fn [[x y]] (+ y 0)) living)
        rows  (for [x (range (first min-x) (inc (first max-x)))
                    y (range (second min-y) (inc (second max-y)))]
                (if (living [x y]) \# \-))]
    (apply str (apply concat (interpose "\n" (partition (first max-x) rows))))))



(defn string-to-board [s])

;;;; Test Code ;;;;
  
(def living #{[1 1] [1 2] [3 1] [3 2] [4 2]})
(def living2 #{[-2 -2] [2 2]})

(println (board-to-string living2))