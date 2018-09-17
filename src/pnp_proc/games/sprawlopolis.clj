(ns pnp-proc.games.sprawlopolis
  "Recipe for Sprawolopolis, based on
   SPRAWLOPOLIS-PNP.pdf
   SPRAWLOPOLIS-CZ-PNP.pdf
   SPRAWLOPOLIS-POI-PNP.pdf
   SPRAWLOPOLIS-W-PNP.pdf"
  (:require [mikera.image.core :as img]
            [pnp-proc.pdf :as pdf]
            [pnp-proc.util :as util]
            [pnp-proc.card :as card]
            [pnp-proc.collecting :as collecting]
            [pnp-proc.assembling :as assembling]))

(defn make [output-path main-pdf cz-pdf poi-pdf w-pdf]
  ;; Coordinate data based on images from
  ;; (tools/render-page-to-image "SPRAWLOPOLIS-PNP.pdf" 1 1 "sprawlopolis.png")
  ;; First page (page 0) is the rules.
  ;; Image 0 is scale 197.28, the others are 197.28041. In practice they are
  ;; equal though.
  (let [scale (pdf/get-bitmap-scale main-pdf 1 1)
        top-left-cut [448 188]
        ;; POI and W are slightly smaller (cut lines [422 177] [1126 1157] =
        ;; size 705x981 contra main size 748x1041) but the images are the
        ;; same size as main. But the scale is 0.25646 vs. 0.24177 which
        ;; gives about the same points.
        ;; But there's enough graphics to just use the same resolution.
        ;; Actually, I feel they cut off more than needed.
        ;; TODO: Create an option or new recipe that is more true to the PDF.
        card-dimensions (util/card-dimensions-from-cut-lines top-left-cut
                                                             [1195 1228])
        card-offset (util/relative-offset-from-cut-lines [414 151]
                                                         top-left-cut)
        ;; The list will include the rules on page 1 (two impages.
        ;; Maybe we should handle the rules if we want to resize cards?
        main-images (pdf/get-images-from-pdf main-pdf)
        cz-images (pdf/get-images-from-pdf cz-pdf)
        poi-images (pdf/get-images-from-pdf poi-pdf)
        w-images (pdf/get-images-from-pdf w-pdf)
        ;; Now calculate the offset to get rule cards of the same size.
        ;; Center, rounding down.
        rule-card-sample (second poi-images)
        exp-rule-card-offset [(int (/ (- (img/width rule-card-sample)
                                         (first card-dimensions))
                                      2))
                              (int (/ (- (img/height rule-card-sample)
                                         (second card-dimensions))
                                      2))]
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
        main-card-images (->> main-images
                              ;; Skip rules on page 1.
                              (drop 2)
                              ;; And change order.
                              (util/normalize-group-layout [1 0 2
                                                            3 4 5]))
        ;; CZ has same order and two columns of uneven size (a gap in the lower.
        cz-card-images (util/normalize-group-layout [1 0 2
                                                     3 nil 4]
                                                    cz-images)
        poi&w-layout [[1 0 nil]
                      [nil 0 1]]
        poi-card-images (util/normalize-group-layout poi&w-layout poi-images)
        w-card-images (util/normalize-group-layout poi&w-layout w-images)
        ;; Collect all cards.
        main-cards (collecting/collect-cards main-card-images [3 2])
        cz-cards (collecting/collect-cards cz-card-images [3 2])
        poi-cards (collecting/collect-cards poi-card-images [3 1])
        w-cards (collecting/collect-cards w-card-images [3 1])
        ;; Separate to register the different offsets.
        exp-rule-cards-raw [(last cz-cards)
                            (first poi-cards)
                            (first w-cards)]
        exp-rule-cards (card/add-card-info exp-rule-cards-raw
                                           exp-rule-card-offset)
        norm-cards-raw (concat main-cards (drop-last cz-cards)
                               [(last poi-cards) (last w-cards)])
        norm-cards (card/add-card-info norm-cards-raw
                                       card-offset)
        cards (concat norm-cards exp-rule-cards)
        output-images-cards (assembling/assemble-cards cards 4
                                                       [30 30] [30 30]
                                                       card-dimensions)]
    ;; The source must not be closed before the destination when copying.
    (pdf/with-open-doc [org-doc main-pdf]
      (pdf/with-make-pdf [doc output-path]
        ;; Copy the rule page directly from the source.
        (pdf/add-page-from! doc org-doc 0)
        (pdf/add-images-as-pages! doc output-images-cards scale)))))
