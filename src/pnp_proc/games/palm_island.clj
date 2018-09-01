(ns pnp-proc.games.palm-island
  "Recipe for Palm Island, based on Palm-Island-Print-and-Play1.4.6.pdf"
  (:require [pnp-proc.pdf     :as pdf]
            [pnp-proc.worker  :as worker]
            [pnp-proc.util    :as util]))

(defn make [pdf]
  (let [second-card-offset [903 1130]
        card-dimensions (util/card-dimensions-from-coords second-card-offset [1646 2169])
        offset-coords (util/absolute-offset-from-second-card second-card-offset card-dimensions)
        images (pdf/get-images-from-pdf pdf)
        img-sets (partition 2 images)
        cards (worker/cards-from-image-sets img-sets
                                            offset-coords card-dimensions [3 3])]))
