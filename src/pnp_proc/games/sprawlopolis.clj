(ns pnp-proc.games.sprawlopolis
  "Recipe for Sprawolopolis, based on SPRAWLOPOLIS-PNP.pdf"
  (:require [pnp-proc.pdf         :as pdf]
            [pnp-proc.util        :as util]
            [pnp-proc.collecting  :as collecting]))

(defn make [main-pdf cz-pdf poi-pdf w-pdf]
  ;; Coordinate data based on images from
  ;; (tools/render-page-to-image "SPRAWLOPOLIS-PNP.pdf" 1 0 "sprawlopolis.png")
  (let [top-left-cut [448 188]
        card-dimensions (util/card-dimensions-from-cut-lines top-left-cut
                                                             [1195 1228])
        card-offset (util/relative-offset-from-cut-lines [414 151]
                                                         top-left-cut)
        ;; The list will include the rules on page 1 (two impages.
        ;; Maybe we should handle the rules if we want to resize cards?
        main-images (pdf/get-images-from-pdf main-pdf)
        ;; The card images are weirdly but consistently arranged
        ;; with the first and second in the first row switching place.
        ;; But this means that when reversing the sets of backs to match
        ;; the fronts we match the top rows in the wrong order. Instead of
        ;; the following sets:
        ;;  left   middle  right
        ;;  right  middle  left
        ;; we get
        ;;  middle  left  right
        ;;  right   left  middle
        card-images (->> main-images
                         ;; Skip rules on page 1.
                         (drop 2)
                         ;; And change order.
                         (util/change-group-order [1 0 2 3 4 5]))
        cards (collecting/collect-cards card-images [3 2])]))
