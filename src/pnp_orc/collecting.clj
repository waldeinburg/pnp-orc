(ns pnp-orc.collecting
  (:require [pnp-orc.util :as util]
            [mikera.image.core :as img]))

(defn- order-backs [backs columns]
  (let [rows (partition columns backs)]
    (mapcat reverse rows)))

(defn- rotate-backs [backs]
  (map #(img/rotate % 180) backs))

(defn collect-cards
  "Assumption: paper is portrait / long edge or landscape / short edge"
  ([fronts backs columns]
   (let [fronts-real (util/remove-nils fronts)
         backs-real (-> backs
                        (order-backs columns)
                        ;; When the backs are ordered according to columns
                        ;; it's safe to remove nils inserted to normalize
                        ;; layout.
                        util/remove-nils
                        rotate-backs)]
     (map vector fronts-real backs-real)))
  ([images [columns rows]]
   (let [set-size (* rows columns)
         sets (partition set-size images)
         sets-count (count sets)
         sets-i (partial nth sets)
         fronts (mapcat sets-i (range 0 sets-count 2))
         backs (mapcat sets-i (range 1 sets-count 2))]
     (collect-cards fronts backs columns))))
