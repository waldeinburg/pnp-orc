(ns pnp-orc.background
  (:require [mikera.image.core :as img])
  (:import java.awt.Color))

;;; Functions for drawing the background with the cut lines.
;;; The cut lines are meant to be cut away, i.e., one can place the ruler so
;;; that the cut lines is just visible. The card image is thus supposed to be
;;; surrounded by lines

(defn- create-white-image [width height]
  ;; Create background without alpha channels.
  ;; At the time of writing mikera/imagez only allows loading images
  ;; as ARGB but in case this changes we don't want alpha.
  (let [bg (img/new-image width height false)]
    (img/fill! bg Color/white)))

(defn- get-width [number-of-cards
                  margin-x card-width spacing-x]
  (+ (* 2 margin-x)
     ;; 2 for the cut lines
     (* number-of-cards (+ 2 card-width))
     (* (dec number-of-cards) spacing-x)))

(defn- get-height [margin-y card-height fold-margin-y]
  ;; 1 pixel for the fold line and 4 for the cut lines
  (+ 4
     (* 2 (+ margin-y
             card-height
             fold-margin-y))))

(defn- get-vertical-lines [number-of-cards
                           margin-x card-width spacing-x]
  (mapcat #(let [left-edge (+ margin-x
                              (* % (+ 2 card-width spacing-x)))]
             [left-edge
              (+ 1 left-edge card-width)])
          (range number-of-cards)))

(defn- get-horizontal-lines [margin-y card-height fold-margin-y]
  (reductions +
              [margin-y (inc card-height)               ; front
               (inc fold-margin-y)                      ; fold line
               (inc fold-margin-y) (inc card-height)])) ; back

(defn create-background [number-of-cards
                         [cut-margin-x cut-margin-y]
                         [card-width card-height]
                         [cut-spacing-x cut-fold-margin-y]]
  (let [width (get-width number-of-cards cut-margin-x card-width cut-spacing-x)
        height (get-height cut-margin-y card-height cut-fold-margin-y)
        max-x (dec width)
        max-y (dec height)
        ver-lines (get-vertical-lines number-of-cards
                                      cut-margin-x card-width cut-spacing-x)
        hor-lines (get-horizontal-lines cut-margin-y card-height cut-fold-margin-y)
        bg (create-white-image width height)
        gr (img/graphics bg)]
    (.setColor gr Color/black)
    (doseq [x ver-lines]
      (.drawLine gr x 0 x max-y))
    (doseq [y hor-lines]
      (.drawLine gr 0 y max-x y))
    ;; Return image
    bg))
