(ns pnp-proc.games.sprawlopolis
  "Recipe for Sprawolopolis, based on SPRAWLOPOLIS-PNP.pdf"
  (:require [pnp-proc.pdf :as pdf]
            [pnp-proc.tools :as tools]
            [pnp-proc.util :as util]
            [pnp-proc.collecting :as collecting]
            [pnp-proc.assembling :as assembling]
            [pnp-proc.file :as file]))

(defn make [output-path main-pdf cz-pdf poi-pdf w-pdf]
  ;; Coordinate data based on images from
  ;; (tools/render-page-to-image "SPRAWLOPOLIS-PNP.pdf" 1 1 "sprawlopolis.png")
  ;; First page (page 0) is the rules.
  ;; Image 0 is scale 197.28, the others are 197.28041. In practice they are
  ;; equal though.
  (let [scale (tools/get-bitmap-scale main-pdf 1 1)
        top-left-cut [448 188]
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
        cards (collecting/collect-cards card-images [3 2])
        output-images (assembling/assemble-cards cards 4 [30 30] [30 30]
                                                 card-dimensions card-offset)]
    (pdf/images->pdf output-path output-images scale)))
