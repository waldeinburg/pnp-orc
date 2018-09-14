(ns pnp-proc.games.sprawlopolis
  "Recipe for Sprawolopolis, based on
   SPRAWLOPOLIS-PNP.pdf
   SPRAWLOPOLIS-CZ-PNP.pdf
   SPRAWLOPOLIS-POI-PNP.pdf
   SPRAWLOPOLIS-W-PNP.pdf"
  (:require [mikera.image.core :as img]
            [pnp-proc.pdf :as pdf]
            [pnp-proc.tools :as tools]
            [pnp-proc.util :as util]
            [pnp-proc.card :as card]
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
                                                       card-dimensions)
        ;; Now we're at it, include the rules in the PDF.
        ;; This one is actually tricky: The images are off by one pixel to
        ;; line up the images.
        ;; There's 1 px excess material at the top of the image and no
        ;; horizontal excess material based on cut lines; actually they are
        ;; off a little, including blank space to the left.
        ;; The "page" size is 750 (3000/4) x 1050 pixels, which is
        ;; slightly larger (card size 748x1041). The front page is actually
        ;; 755 pixels. The scale is slightly smaller, though (0.24000 vs.
        ;; 0.24177).
        ;; By using the same resolution we would get rules which are
        ;; 8 * 0.24177 / 72 * 25.4 = 0.7 mm too high. And the "correct"
        ;; scale will ensure that the 755 px front page is the correct size.
        ;; The order of front/back is still reversed.
        ;; The lower image is rotated so we can treat it as if on a new page.
        ;; There's only two images, but we are still dealing with lists.
        rules-scale (tools/get-bitmap-scale main-pdf 0 1)
        rules-images (reverse (take 2 main-images))
        rules-cards (collecting/collect-cards rules-images [1 1])
        rules-sample (first rules-images)
        rules-dimensions [(img/width rules-sample)
                          (dec (img/height rules-sample))]
        rules-offset [0 1]
        output-images-rules (assembling/assemble-cards rules-cards 1
                                                       [30 30] [30 30]
                                                       rules-dimensions
                                                       rules-offset)]
    (pdf/with-make-pdf [doc output-path]
      (pdf/add-images-as-pages! doc output-images-rules rules-scale)
      (pdf/add-images-as-pages! doc output-images-cards scale))))
