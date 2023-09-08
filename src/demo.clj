;; Game of life rules
;; 1) A living cell with 2 > neighbors > 3 dies
;; 2) A dead cell with 3 neighbors comes to life
;; 3) Any other cell remains in its current state

(ns demo
  (:require [clojure.string :as str]))

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
  (let [xmin (first  (apply min-key first living))
        xmax (first  (apply max-key first living))
        ymin (second (apply min-key second living))
        ymax (second (apply max-key second living))
        rows  (for [y (range ymin (inc ymax))
                    x (range xmin (inc xmax))]
                (if (living [x y]) \# \-))]
    
    (apply str (apply concat (interpose "\n"
                                        (reverse (partition (count (range xmin (inc xmax))) rows)))))))



(defn string-to-board [s])

;;;; Test Code ;;;;

(comment
  (def living #{[1 1] [1 2] [3 1] [3 2] [4 2]})
  (def living2 #{[-2 -2] [1 10] [2 2]})

  (defn board-to-string [living]
    (let [xmin (first  (apply min-key first living))
          xmax (first  (apply max-key first living))
          ymin (second (apply min-key second living))
          ymax (second (apply max-key second living))
          rows  (for [y (range ymin (inc ymax))
                      x (range xmin (inc xmax))]
                  (if (living [x y]) \# \-))]
      (str/join "\n" (reverse (partition (inc (- xmax xmin)) rows))))))

  (board-to-string living2)

  (println (board-to-string living2))

(comment 
  (def s "---#-\n-----\n-----\n-----\n-----\n-----\n-----\n-----\n----#\n-----\n-----\n-----\n#----")

  (defn string-to-board [s]
    (let [s (reverse s)
          no-newline (str/split s #"\n")
          split (map (fn [x] str/split x #"") no-newline)
          line-length (count (str/split (first no-newline) #""))]
     (for [y (range (count no-newline))]
       (for [x (range line-length)]
         [x y]
        ;;  (if (= (nth split (dec (+ x y))) \#) [x y] nil)
         ))
     ))
  

  (string-to-board s) 
  (reverse s)
  )