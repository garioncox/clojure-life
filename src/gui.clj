(ns gui
  (:require [game_of_life]
            [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 100)
  (q/background 200))

(defn draw []
  (q/stroke (q/random 255))             ;; Set the stroke colour to a random grey
  (q/stroke-weight (q/random 10))       ;; Set the stroke thickness randomly
  (q/fill (q/random 255))               ;; Set the fill colour to a random grey

  (let [diam (q/random 100)             ;; Set the diameter to a value between 0 and 100
        x    (q/random (q/width))       ;; Set the x coord randomly within the sketch
        y    (q/random (q/height))]     ;; Set the y coord randomly within the sketch
    (q/ellipse x y diam diam)))         ;; Draw a circle at x y with the correct diameter

(defn run [opts]
  (q/defsketch example                  ;; Define a new sketch named example
    :title "Oh so many grey circles"    ;; Set the title of the sketch
    :settings #(q/smooth 2)             ;; Turn on anti-aliasing
    :setup setup                        ;; Specify the setup fn
    :draw draw                          ;; Specify the draw fn
    :size [900 900]))  