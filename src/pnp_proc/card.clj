(ns pnp-proc.card
  (:require [mikera.image.core :as img]))

;; CardImage holds an image and offset information.
;; The assemble function must assume equal size of all cards to create cut
;; lines, and this information is not included in the record.
(defrecord CardImage [img offset])

(defn add-card-info [cards offset]
  "Take a sequence of cards (vector front-img back-img) and replace with
   CardImage, i.e., add data about offset and dimensions."
  (map (fn [images]
         (map #(CardImage. % offset) images))
       cards))

(defn crop [card dimensions]
  "Crop a CardImage and return a new CardImage with offset 0 0"
  (CardImage. (apply img/sub-image (:img card)
                     (concat (:offset card) dimensions))
              [0 0]))
