(ns pnp-proc.games.palm-island
  "Recipe for Palm Island, based on Palm-Island-Print-and-Play1.4.6.pdf"
  (:require [pnp-proc.pdf     :as pdf]
            [pnp-proc.worker  :as worker]
            [pnp-proc.calc    :as calc]))

(defn make [pdf]
  (let [card-dimensions [744 1040]
        offset-coords (calc/upper-left-from-second [903 1130] card-dimensions)
        images (pdf/get-images-from-pdf pdf)
        img-sets (partition 2 images)
        cards (worker/cards-from-image-sets img-sets
                                            offset-coords card-dimensions [3 3])]))
