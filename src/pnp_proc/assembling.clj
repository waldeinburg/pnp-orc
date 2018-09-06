(ns pnp-proc.assembling
  (:require [pnp-proc.background :as background]
            [mikera.image.core :as img])
  (:import (java.awt Graphics2D Image)))

(defn assemble-cards
  "Take pairs of front and back and properties for creating a background
   and placing the card images.
   If card dimensions are the same as the image dimensions the dimensions
   and offset of the actual card can be omitted.
   Return a vector of images for print."
  ([cards
    cards-per-page
    margins
    spacings]
   (let [sample (first (first cards))
         card-width (img/width sample)
         card-height (img/height sample)]
     (assemble-cards cards cards-per-page margins spacings
                     [card-width card-height] [0 0])))
  ([cards
    cards-per-page
    [margin-x margin-y]
    [spacing-x fold-margin-y]
    [card-width card-height]
    [card-rel-offset-x card-rel-offset-y]]
   (if (empty? cards)
     []
     (let [sample (first (first cards))
           card-img-width (img/width sample)
           card-img-height (img/height sample)
           right-excess (- card-img-width card-width card-rel-offset-x)
           bottom-excess (- card-img-height card-height card-rel-offset-y)
           ;; The margin to the cut lines should be enough to ensure the
           ;; wanted margin on the side with the largest excess, no less.
           ;; Normally the card will be centered on the image, so we don't
           ;; make things more complicated by differing between sides.
           cut-margin-x (+ margin-x (max card-rel-offset-x right-excess))
           cut-margin-y (+ margin-y (max card-rel-offset-y bottom-excess))
           cut-spacing-x (+ spacing-x card-rel-offset-x right-excess)
           cut-fold-margin-y (+ fold-margin-y bottom-excess)
           bg (background/create-background cards-per-page
                                            [cut-margin-x cut-margin-y]
                                            [card-width card-height]
                                            [cut-spacing-x cut-fold-margin-y])
           new-bg #(img/copy bg)
           front-y margin-y
           back-y (+ margin-y
                     card-img-height
                     (* 2 margin-y)
                     1)]
       (loop [images [(new-bg)]
              rest-cards cards
              card-i 0]
         (let [^Graphics2D grph (img/graphics (last images))
               card (first rest-cards)
               front (first card)
               back (second card)
               x (+ (- cut-margin-x card-rel-offset-x)
                    (* card-i (+ card-width cut-spacing-x)))
               draw (fn [src y]
                      ;; The type hints are for overload disambiguation.
                      (.drawImage grph ^Image src ^Integer x ^Integer y nil))]
           (draw front front-y)
           (draw back back-y)
           (let [new-rest-cards (rest rest-cards)
                 new-card-i (inc card-i)]
             (if (empty? rest-cards)
               images
               (if (= new-card-i cards-per-page)
                 (recur (conj images (new-bg)) new-rest-cards 0)
                 (recur images new-rest-cards new-card-i))))))))))
