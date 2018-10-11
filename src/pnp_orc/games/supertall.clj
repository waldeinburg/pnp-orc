(ns pnp-orc.games.supertall
  "Recipe for Supertall, based on
   SUPERTALL-PNP.pdf
   SUPERTALL_MB-PNP.pdf"
  (:require [pnp-orc.pdf :as pdf]
            [pnp-orc.util :as util]
            [pnp-orc.collecting :as collecting]
            [pnp-orc.assembling :as assembling]
            [mikera.image.core :as img]
            [clojure.math.numeric-tower :as math]))

(defn make [output-path main-pdf mb-pdf]
  ;; Same structure as Sprawlopolis except we only need one back. However,
  ;; it's easier to just use the layout instead of reusing one back image.
  ;; Cf. the Sprawlopolis recipe for comments.
  ;; They seem to have made a two pixel error in the y-offset of the first card
  ;; that we can ignore.
  ;; The MB expansion is structured as main except the rules:
  ;; In the MB expansion, only the rule card is a smaller bitmap and the cut
  ;; lines make no sense as they cut the title and designer names.
  ;; The card dimensions are 748x1041 px and the MB rules bitmaps are 750x1026px.
  ;; But if the rule page is rendered with the same DPI as the card page we see
  ;; that the image of the rules is the same size as the image as the cards.
  ;; The rules was thus scaled incorrectly. We fix this.
  (let [cards-scale (pdf/get-bitmap-scale main-pdf 1 1)
        mb-rule-card-scale (pdf/get-bitmap-scale mb-pdf 0 0)
        top-left-cut [448 188]
        card-dimensions (util/card-dimensions-from-cut-lines top-left-cut
                                                             [1195 1228])
        card-offset (util/relative-offset-from-cut-lines [414 151]
                                                         top-left-cut)
        main-images (pdf/get-images-from-pdf main-pdf)
        mb-images (pdf/get-images-from-pdf mb-pdf)
        ;; The layout doesn't matter because the backs are the same.
        main-card-images (drop 2 main-images)
        mb-card-images (drop 2 mb-images)
        card-images (concat main-card-images mb-card-images)
        cards (collecting/collect-cards card-images [3 2])
        output-images-cards (assembling/assemble-cards cards 4
                                                       [30 30] [0 30]
                                                       card-dimensions
                                                       card-offset)
        ;; The MB rules is just one card.
        mb-rule-card-images (take 2 mb-images)
        mb-rule-card (collecting/collect-cards mb-rule-card-images [1 1])
        ;; Horizontal bleed only.
        mb-rc-img-sample (first mb-rule-card-images)
        mb-rc-height (img/height mb-rc-img-sample)
        mb-rc-width (* mb-rc-height (apply / card-dimensions))
        mb-rc-offset-x (math/round (/ (- (img/width mb-rc-img-sample)
                                         mb-rc-width)
                                      2))
        output-image-mb-rule-card (assembling/assemble-cards mb-rule-card 1
                                                             [30 30] [0 30]
                                                             [mb-rc-width
                                                              mb-rc-height]
                                                             [mb-rc-offset-x 0])]
    (pdf/with-open-doc [org-doc main-pdf]
      (pdf/with-make-pdf [doc output-path]
        (pdf/add-page-from! doc org-doc 0)
        (pdf/add-images-as-pages! doc output-image-mb-rule-card mb-rule-card-scale)
        (pdf/add-images-as-pages! doc output-images-cards cards-scale)))))