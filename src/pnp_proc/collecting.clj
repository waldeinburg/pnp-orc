(ns pnp-proc.collecting
  (:require [mikera.image.core :as img]))

(defn- order-backs [backs columns]
  (let [rows (partition columns backs)]
    (mapcat reverse rows)))

(defn- rotate-backs [backs]
  (map #(img/rotate % 180) backs))

(defn collect-cards [fronts backs columns]
  "Assumption: paper is portrait and should be printed long edge"
  (let [backs (-> backs order-backs rotate-backs)]
    (map vector fronts backs)))
