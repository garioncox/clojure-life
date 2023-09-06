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
     (count
      (filter #(contains? (set living) %)
              (neighbors-of [x y])))))
  ([living]
   (fn [cell] (living-neighbors cell living))))

(defn will-live? [[x y] living]
  (let [neighbors (living-neighbors [x y] living)]
    (if (and (living [x y]) (< 1 neighbors) (> 4 neighbors)) true
        (if (and (not= (contains? (set living) [x y]) true) (= 3 neighbors)) true false))))

(defn next-generation [living]
  (filter (fn [cell] (will-live? cell living)) (distinct (apply concat (map neighbors-of living)))))



;;;;; Testing Code ;;;;;

;; (will-live? [2 2] (neighbors-of [-2 -2]))
;; (will-live? [2 2] #{[2 3] [3 3] [1 2] [1 1]})
;; (will-live? [2 2] #{[2 3] [3 3] [1 2]})
;; (will-live? [2 2] #{[2 3] [3 3]})
;; (will-live? [2 2] #{[2 3]})

;; (next-generation [[1 1] [1 2] [3 1] [3 2] [4 2]])
;; (next-generation (set '([1 1] [1 2] [3 1] [3 2] [4 2])))