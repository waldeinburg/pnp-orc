(ns pnp-proc.worker
  (:require [mikera.image.core :as img]
            [pnp-proc.segmenting :as segmenting]
            [pnp-proc.collecting :as collecting]
            [pnp-proc.assembling :as assembling]))

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

(defn- load-images [paths]
  "Load images, pairing fronts and backs"
  (partition 2 (map img/load-image paths)))

(defn load-indexed-images [fmt number-of-images]
  (let [paths (map #(format fmt %)
                   (range number-of-images))]
    (load-images paths)))

(defn save-images [images fmt]
  (doseq [i (range (count images))]
    (img/save (nth images i) (format fmt i))))
