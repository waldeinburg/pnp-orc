(ns pnp-proc.assembling
  (:require [mikera.image.core :as img]
            [pnp-proc.background :as background]
            [pnp-proc.card :as card])
  (:import (java.awt Graphics2D Image)))

(defn assemble-cards
  "Take pairs of front and back and properties for creating a background
   and placing the card images.
   If card dimensions are the same as the image dimensions the dimensions
   and offset of the actual card can be omitted. In that case, the cards
   must be image pairs only, not CardImage instances.
   Return a vector of images for print."
  ([cards
    cards-per-page
    margins
    spacings]
   (let [sample (first (first cards))
         card-width (img/width sample)
         card-height (img/height sample)]
     (assemble-cards cards
                     cards-per-page margins spacings
                     [card-width card-height]
                     [0 0])))
  ([cards
    cards-per-page
    margins
    spacings
    card-dimensions
    card-offset]
   (assemble-cards (card/add-card-info cards card-offset)
                   cards-per-page margins spacings card-dimensions))
  ([cards
    cards-per-page
    [margin-x margin-y]
    [spacing-x fold-margin-y]
    [card-width card-height]]
   (if (empty? cards)
     []
     (let [cards-flat (apply concat cards)
           max-card-rel-offset-x (apply max
                                        (map #(first (:offset %)) cards-flat))
           max-card-rel-offset-y (apply max
                                        (map #(second (:offset %)) cards-flat))
           max-right-bleed (apply max (map #(- (img/width (:img %))
                                               card-width
                                               (first (:offset %)))
                                           cards-flat))
           max-bottom-bleed (apply max (map #(- (img/height (:img %))
                                                card-height
                                                (second (:offset %)))
                                            cards-flat))
           ;; The margin to the cut lines should be enough to ensure the
           ;; wanted margin on the side with the largest bleed, no less.
           ;; Normally the card will be centered on the image, so we don't
           ;; make things more complicated by differing between sides.
           cut-margin-x (+ margin-x (max max-card-rel-offset-x max-right-bleed))
           cut-margin-y (+ margin-y (max max-card-rel-offset-y max-bottom-bleed))
           cut-spacing-x (+ spacing-x max-card-rel-offset-x max-right-bleed)
           cut-fold-margin-y (+ fold-margin-y max-bottom-bleed)
           bg (background/create-background cards-per-page
                                            [cut-margin-x cut-margin-y]
                                            [card-width card-height]
                                            [cut-spacing-x cut-fold-margin-y])
           new-bg #(img/copy bg)]
       (loop [cur-img (new-bg)
              images []
              rest-cards cards
              card-i 0]
         ;; Add card to current image.
         (let [^Graphics2D grph (img/graphics cur-img)
               card (first rest-cards)
               front (first card)
               back (second card)
               front-y-fn (fn [offset-y]
                            (- cut-margin-y offset-y))
               back-y-fn (fn [offset-y]
                           (- (+ cut-margin-y
                                 card-height
                                 (* 2 cut-fold-margin-y)
                                 1)
                              offset-y))
               draw (fn [src y-fn]
                      (let [offset (:offset src)
                            x (+ (- cut-margin-x (first offset))
                                 (* card-i (+ card-width cut-spacing-x)))
                            y (y-fn (second offset))]
                        ;; The type hints are for overload disambiguation.
                        (.drawImage grph ^Image (:img src)
                                    ^Integer x ^Integer y nil)))]
           (draw front front-y-fn)
           (draw back back-y-fn)
           (let [new-rest-cards (rest rest-cards)
                 new-card-i (inc card-i)]
             ;; It's important to make the done check here. If we do it at the
             ;; beginning of the loop and the final card fills the page we
             ;; would add a blank page. Empty card argument is checked before
             ;; the loop.
             (if (empty? new-rest-cards)
               ;; Done. Add final image and return.
               (conj images cur-img)
               ;; Continue with new or same page.
               (if (= new-card-i cards-per-page)
                 (recur (new-bg) (conj images cur-img) new-rest-cards 0)
                 (recur cur-img images new-rest-cards new-card-i))))))))))
