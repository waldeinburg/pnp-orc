(ns pnp-orc.worker
  (:require [pnp-orc.segmenting :as segmenting]
            [pnp-orc.collecting :as collecting]))

(defn- cards-from-img-set [[fronts-img backs-img]
                           offset-coords card-dimensions
                           [columns rows]]
  (let [get-cards #(segmenting/get-cards %
                                         offset-coords
                                         card-dimensions
                                         [columns rows])
        fronts (get-cards fronts-img)
        backs (get-cards backs-img)]
    (collecting/collect-cards fronts backs columns)))

(defn cards-from-image-sets [page-sets
                             offset-coords card-dimensions layout]
  (mapcat #(cards-from-img-set % offset-coords card-dimensions layout)
          page-sets))
