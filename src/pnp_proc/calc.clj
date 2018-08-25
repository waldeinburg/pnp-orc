(ns pnp-proc.calc)

(defn upper-left-from-second [[second-offset-x second-offset-y]
                              [card-width card-height]]
  [(- second-offset-x card-width)
   (- second-offset-y card-height)])

;; TODO: maybe we should get the PDRectangle of the original
;; PDPage instead of using the image as point of departure.
;; This will be easier to handle for e.g. Sprawlopolis which
;; has several images in one page.
(defn optimal-assemble-parameters [[img-width img-height]
                                   [card-width card-height]]
  "Calculate margins and number of cards per page based on data
   from the original files")
