(ns pnp-proc.worker
  (:require [pnp-proc.segmenting :as segmenting]
            [pnp-proc.collecting :as collecting]))

(defn- cards-from-page-set [[fronts-img backs-img]
                            offset-coords card-dimensions
                            [columns rows]]
  (let [get-cards #(segmenting/get-cards %
                                         offset-coords
                                         card-dimensions
                                         [columns rows])
        fronts (get-cards fronts-img)
        backs (get-cards backs-img)]
    (collecting/collect-cards fronts backs columns)))

(defn cards-from-page-sets [page-sets
                            offset-coords card-dimensions layout]
  (mapcat #(cards-from-page-set % offset-coords card-dimensions layout)
          page-sets))
