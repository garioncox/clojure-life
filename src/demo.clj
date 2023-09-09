;; Game of life rules
;; 1) A living cell with 2 > neighbors > 3 dies
;; 2) A dead cell with 3 neighbors comes to life
;; 3) Any other cell remains in its current state

(ns demo
  (:require [clojure.string :as string]))

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
      (apply str (apply concat (interpose "\n" (reverse (partition (inc (- xmax xmin)) rows)))))))



(defn string-to-board [s])

;;;; Test Code ;;;;

(comment
  (board-to-string #{[-2 -2] [1 10] [2 2]})

  (def s "---#-\n-----\n-----\n-----\n-----\n-----\n-----\n-----\n----#\n-----\n-----\n-----\n#----")

  (defn string-to-board [s]
    (let [no-newline (reverse (clojure.string/split s #"\n"))
          split '(map seq no-newline)
          rows (count (first no-newline))
          cols (count split)]
      
      (filter identity
              (for [y (range 13)
                    x (range 5)]
                (if (= (get-in split [x y]) \#) [x y] (get-in split [x y]))))
        ;;  (if (= (nth split (dec (+ x y))) \#) [x y] nil)
      ))

  (def nln ["#----" "-----" "-----" "-----" "----#" "-----" "-----" "-----" "-----" "-----" "-----" "-----" "---#-"])

  (map seq nln)

  (string-to-board s)

  (defn mystery [s]
    (for [y (range 13)
          x (range 5)]
      (if (= (get-in s [x y]) \#) [x y] (get-in s [x y]))))


  (mystery ["foo"])
  )