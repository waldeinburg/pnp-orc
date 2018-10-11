(ns pnp-orc.file
  (:require [mikera.image.core :as img]))

(defn- load-images [paths]
  "Load images, pairing fronts and backs"
  (partition 2 (map img/load-image paths)))

(defn default-fmt [folder]
  (str folder "/img-%03d.png"))

(defn load-indexed-images [fmt number-of-images]
  (let [paths (map #(format fmt %)
                   (range number-of-images))]
    (load-images paths)))

(defn save-images [images fmt]
  (doseq [i (range (count images))]
    (img/save (nth images i) (format fmt i))))
