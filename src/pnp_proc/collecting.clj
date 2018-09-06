(ns pnp-proc.collecting
  (:require [mikera.image.core :as img]))

(defn- order-backs [backs columns]
  (let [rows (partition columns backs)]
    (mapcat reverse rows)))

(defn- rotate-backs [backs]
  (map #(img/rotate % 180) backs))

(defn collect-cards
  "Assumption: paper is portrait and should be printed long edge"
  ([fronts backs columns]
   (let [backs (-> backs rotate-backs (order-backs columns))]
     (map vector fronts backs)))
  ([images [columns rows]]
   (let [set-size (* rows columns)
         sets (partition set-size images)
         sets-count (count sets)
         sets-i (partial nth sets)
         fronts (mapcat sets-i (range 0 sets-count 2))
         backs (mapcat sets-i (range 1 sets-count 2))]
     (collect-cards fronts backs columns))))
