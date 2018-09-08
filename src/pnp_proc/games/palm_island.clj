(ns pnp-proc.games.palm-island
  "Recipe for Palm Island, based on Palm-Island-Print-and-Play1.4.6.pdf"
  (:require [pnp-proc.pdf :as pdf]
            [pnp-proc.tools :as tools]
            [pnp-proc.worker :as worker]
            [pnp-proc.assembling :as assembling]
            [pnp-proc.util :as util]
            [pnp-proc.file :as file]))

(defn make [output-path pdf]
  (let [scale (tools/get-bitmap-scale pdf 0 0)
        second-card-offset [903 1130]
        card-dimensions (util/card-dimensions-from-coords second-card-offset [1646 2169])
        offset-coords (util/absolute-offset-from-second-card second-card-offset card-dimensions)
        images (pdf/get-images-from-pdf pdf)
        img-sets (partition 2 images)
        cards (worker/cards-from-image-sets img-sets
                                            offset-coords card-dimensions [3 3])
        output-images (assembling/assemble-cards cards 4 [30 30] [30 30])]
    (pdf/images->pdf output-path output-images scale)))
