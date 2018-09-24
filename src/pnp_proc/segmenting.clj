(ns pnp-proc.segmenting
  (:require [mikera.image.core :as img]))

(defn- get-cards-coordinates [[offset-x offset-y]
                              [card-width card-height]
                              [spacing-x spacing-y]
                              [columns rows]]
  (let [card-width-full (+ card-width spacing-x)
        card-height-full (+ card-height spacing-y)]
    (loop [c 0
           r 0
           coord-sets []]
      (if (= r rows)
        coord-sets
        (let [new-c (mod (inc c) columns)
              new-r (if (zero? new-c)
                      (inc r)
                      r)
              coords [(+ offset-x (* card-width-full c))
                      (+ offset-y (* card-height-full r))]]
          (recur new-c new-r (conj coord-sets coords)))))))

(defn get-cards
  ([image
    offset-coords
    card-dimensions
    layout]
   (get-cards image offset-coords card-dimensions [0 0] layout))
  ([image
    offset-coords
    [card-width card-height]
    spacings
    layout]
   (map #(img/sub-image image (first %) (second %) card-width card-height)
        (get-cards-coordinates offset-coords
                               [card-width card-height]
                               spacings
                               layout))))
