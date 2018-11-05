(ns pnp-orc.games.potions-class
  "Recipe for Potions Class, based on POTIONS_CLASS-PNP.pdf"
  (:require [pnp-orc.pdf :as pdf]
            [pnp-orc.util :as util]
            [pnp-orc.card :as card]
            [pnp-orc.collecting :as collecting]
            [pnp-orc.assembling :as assembling])
  (:import (org.apache.pdfbox.pdmodel PDDocument)))

(defn make [output-path pdf]
  ;; Same structure as Sprawlopolis and Supertall
  (let [scale (pdf/get-bitmap-scale pdf 1 1)
        top-left-cut [448 188]
        card-dimensions (util/card-dimensions-from-cut-lines top-left-cut
                                                             [1195 1228])
        card-offset (util/relative-offset-from-cut-lines [414 151]
                                                         top-left-cut)
        images (pdf/get-images-from-pdf pdf)
        card-images (->> images
                         ;; Skip rules on page 1.
                         (drop 2)
                         ;; And change order.
                         (util/normalize-group-layout [1 0 2
                                                       3 4 5]))
        cards-raw (collecting/collect-cards card-images [3 2])
        norm-cards (card/add-card-info cards-raw card-offset)
        output-images-cards (assembling/assemble-cards norm-cards 4
                                                       [30 30] [0 30]
                                                       card-dimensions)]
    (pdf/with-open-doc [^PDDocument org-doc pdf]
      (pdf/with-make-pdf [^PDDocument doc output-path]
        (pdf/add-page-from! doc org-doc 0)
        (pdf/add-images-as-pages! doc output-images-cards scale)))))
