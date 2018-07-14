(ns pnp-proc.background
  (:require [mikera.image.core :as img]
            [mikera.image.colours :as col])
  (:import [java.awt.Color]))

(defn- create-white-image [width height]
  (let [bg (img/new-image width height)]
    (img/fill! bg color/white)))

(defn- get-width [number-of-cards
                  margin-x card-width spacing-x]
  (+ (* 2 margin-x)
     (* number-of-cards card-width)
     (* (dec number-of-cards) spacing-x)))

(defn- get-height [margin-y card-height fold-margin-y]
  ;; 1 pixel for the fold line
  (inc (* 2 (+ margin-y
               card-height
               fold-margin-y))))

(defn- get-vertical-lines [number-of-cards
                           margin-x card-width spacing-x]
  (mapcat #(let [left-edge (+ margin-x
                              (* % (+ card-width spacing-x)))]
             [left-edge
              (dec (+ left-edge card-width))])
          (range number-of-cards)))

(defn- get-horizontal-lines [max-y margin-y card-height]
  [margin-y
   (dec (+ margin-y card-height))
   (/ max-y 2)
   (inc (- max-y margin-y card-height))
   (- max-y margin-y)])

(defn create-background [number-of-cards
                         [margin-x margin-y]
                         [card-width card-height]
                         [spacing-x fold-margin-y]]
  (let [width (get-width number-of-cards margin-x card-width spacing-x)
        height (get-height margin-y card-height fold-margin-y)
        max-x (dec width)
        max-y (dec height)
        ver-lines (get-vertical-lines number-of-cards margin-x card-width spacing-x)
        hor-lines (get-horizontal-lines max-y margin-y card-height)
        bg (create-white-image width height)
        gr (img/graphics bg)]
    (.setColor gr Color/black)
    (doseq [x ver-lines]
      (.drawLine gr x 0 x max-y))
    (doseq [y hor-lines]
      (.drawLine gr 0 y max-x y))
    ;; Return image
    bg))
