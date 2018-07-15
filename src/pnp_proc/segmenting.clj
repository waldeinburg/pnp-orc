(ns pnp-proc.segmenting
  (:require [mikera.image.core :as img]))

(defn- get-cards-coordinates [[second-offset-x second-offset-y]
                              [card-width card-height]
                              [columns rows]]
  (let [offset-x (- second-offset-x card-width)
        offset-y (- second-offset-y card-height)]
    (loop [c 0
           r 0
           coord-sets []]
      (if (= r rows)
        coord-sets
        (let [new-c (mod (inc c) columns)
              new-r (if (zero? new-c)
                      (inc r)
                      r)
              coords [(+ offset-x (* card-width c))
                      (+ offset-y (* card-height r))]]
          (recur new-c new-r (conj coord-sets coords)))))))

(defn get-cards [image
                 [second-offset-x second-offset-y]
                 [card-width card-height]
                 [columns rows]]
  (map #(img/sub-image image (first %) (second %) card-width card-height)
       (get-cards-coordinates [second-offset-x second-offset-y]
                              [card-width card-height]
                              [columns rows])))
