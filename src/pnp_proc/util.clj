(ns pnp-proc.util)

;;; The following functions does either basically or exactly the same.
;;; They ensure that recipes can be made by quickly reading coordinates in an
;;; image editor without any change of messing up the math for some reason.
;;; For all of us who are idiots and not afraid of admitting it!

(defn card-dimensions-from-coords [[left-x top-y]
                                   [right-x bottom-y]]
  "Calculate card dimensions from coordinates."
  [(inc (- right-x left-x))
   (inc (- bottom-y top-y))])

(defn absolute-offset-from-second-card [[second-offset-x second-offset-y]
                                        [card-width card-height]]
  "Calculate offset of top left card based on coordinates of second from top
   left card.
   This is easier to determine when there is excess pixels on the outer cards."
  [(- second-offset-x card-width)
   (- second-offset-y card-height)])

(defn relative-offset-from-cut-lines [[image-offset-x image-offset-y]
                                      [left-cut top-cut]]
  "Calculate offset of actual card pixels relative to the image of the card
   based on offset and cut lines of an image containing the card.
   This is used based on result from tools/render-page-to-image
   when PDF's contains single images."
  [(- left-cut image-offset-x)
   (- top-cut image-offset-y)])

(defn card-dimensions-from-cut-lines [[left-cut top-cut]
                                      [right-cut bottom-cut]]
  "Calculate actual card dimensions based on coordinates of cut lines.
   The cut lines are treated as inclusive."
  [(inc (- right-cut left-cut))
   (inc (- bottom-cut top-cut))])


;;; Sequence utilities.

(defn remove-nils [seq]
  (filter (complement nil?) seq))

(defn normalize-group-layout [order seq]
  "Change the order of groups of elements in a sequence based on a sequence of indices.
   Useful for changing the order of images before using collect cards where the reversal
   of the backs will result in wrong pairing of images if the PDF is weird.
   An index kan be nil to mark a gap in the layout which should be filled with nil to
   create the correct pairing in collecting/collect-cards."
  (->> seq
       ;; Divide into groups
       (partition (count (remove-nils order)))
       ;; Switch order in each group.
       (map #(map (fn [i]
                    (if (nil? i)
                      nil
                      (nth % i)))
                  order))
       ;; And put back together.
       (apply concat)))

;; TODO: maybe we should get the PDRectangle of the original
;; PDPage instead of using the image as point of departure.
;; This will be easier to handle for e.g. Sprawlopolis which
;; has several images in one page.
(defn optimal-assemble-parameters [[img-width img-height]
                                   [card-width card-height]]
  "Calculate margins and number of cards per page based on data
   from the original files")
