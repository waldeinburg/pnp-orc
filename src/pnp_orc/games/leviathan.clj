(ns pnp-orc.games.leviathan
  "Leviathan based on the Kickstarter backer PnP"
  (:require [pnp-orc.pdf :as pdf]
            [pnp-orc.util :as util]
            [pnp-orc.collecting :as collecting]
            [pnp-orc.assembling :as assembling]))

(defn make [output-path pdf]
  ;; Don't include blank extra cards.
  ;; Cards have no bleed. The images are weirdly ordered.
  (let [scale (pdf/get-bitmap-scale pdf 0 0)
        images (pdf/get-images-from-pdf pdf)
        front-idxs [;; p1
                    4 7 5 1
                    6 0 3 2
                    ;; p2
                    11 13 12 9
                    10 10 14 8
                    ;; p3
                    15 nil nil nil
                    16 nil nil nil]
        ;; Yes, I know repeat, but in this case not using it is more readable.
        back-idxs [;; p4
                   19 19 19 18
                   17 17 17 17
                   ;; p5
                   21 21 21 21
                   20 23 22 22
                   ;; p6
                   nil nil nil 25
                   nil nil nil 25]
        [front-imgs back-imgs] (util/take-idxs images front-idxs back-idxs)
        cards (collecting/collect-cards front-imgs back-imgs 4)
        output-images (assembling/assemble-cards cards 4 [30 30] [30 30])]
    (pdf/images->pdf output-path output-images scale)))
