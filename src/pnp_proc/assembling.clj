(ns pnp-proc.assembling
  (:require [pnp-proc.background :as background]
            [mikera.image.core :as img]))

(defn assemble-cards [cards
                      cards-per-page
                      [margin-x margin-y]
                      [spacing-x fold-margin-y]]
  "Take pairs of front and back and properties for creating a background
   and placing the card images.
   Return a vector of images for print"
  (if (empty? cards)
    []
    (let [sample (first (first cards))
          card-width (img/width sample)
          card-height (img/height sample)
          bg (background/create-background cards-per-page
                                           [margin-x margin-y]
                                           [card-width card-height]
                                           [spacing-x fold-margin-y])
          new-bg #(img/copy bg)
          front-y margin-y
          back-y (+ margin-y
                    card-height
                    (* 2 fold-margin-y)
                    1)]
      (loop [images [(new-bg)]
             rest-cards cards
             card-i 0]
        (let [grph (img/graphics (last images))
              card (first rest-cards)
              front (first card)
              back (second card)
              x (+ margin-x
                   (* card-i (+ card-width spacing-x)))
              draw (fn [src y]
                     (.drawImage grph src x y nil))]
          (draw front front-y)
          (draw back back-y)
          (let [new-rest-cards (rest rest-cards)
                new-card-i (inc card-i)]
            (if (empty? rest-cards)
              images
              (if (= new-card-i cards-per-page)
                (recur (conj images (new-bg)) new-rest-cards 0)
                (recur images new-rest-cards new-card-i)))))))))
