(ns pnp-proc.util)

;;; The following functions does either basically or exactly the same.
;;; They ensure that recipes can be made by quickly reading coordinates in an
;;; image editor without any change of messing up the math for some reason.
;;; For all of us who are idiots and not afraid of admitting it!
;;; The cut line functions assumes that the cut line is cut away and the
;;; coordinates and dimensions should be the resulting image not including the
;;; cut lines.

(defn card-dimensions-from-coords [[left-x top-y]
                                   [right-x bottom-y]]
  "Calculate card dimensions from coordinates."
  [(inc (- right-x left-x))
   (inc (- bottom-y top-y))])

(defn absolute-offset-from-second-card [[second-offset-x second-offset-y]
                                        [card-width card-height]]
  "Calculate offset of top left card based on coordinates of second from top
   left card.
   This is easier to determine when there is bleed pixels on the outer cards."
  [(- second-offset-x card-width)
   (- second-offset-y card-height)])

(defn relative-offset-from-cut-lines [[image-offset-x image-offset-y]
                                      [left-cut top-cut]]
  "Calculate offset of actual card pixels relative to the image of the card
   based on offset and cut lines of an image containing the card.
   This is used based on result from tools/render-page-to-image
   when PDF's contains single images.
   The cut lines are treated as exclusive."
  [(inc (- left-cut image-offset-x))
   (inc (- top-cut image-offset-y))])

(defn card-dimensions-from-cut-lines [[left-cut top-cut]
                                      [right-cut bottom-cut]]
  "Calculate actual card dimensions based on coordinates of cut lines.
   The cut lines are treated as exclusive."
  [(dec (- right-cut left-cut))
   (dec (- bottom-cut top-cut))])


;;; Sequence utilities.

(defn remove-nils [seq]
  (keep identity seq))

(defn normalize-group-layout
  "Change the order of groups of elements in a sequence based on a sequence of indices.
   Useful for changing the order of images before using collect cards where the reversal
   of the backs will result in wrong pairing of images if the PDF is weird.
   An index kan be nil to mark a gap in the layout which should be filled with nil to
   create the correct pairing in collecting/collect-cards."
  ([[front-order back-order :as order] seq]
   (if-not (vector? front-order)
     ;; Allow passing just one vector when front and back are equal.
     (normalize-group-layout [order order] seq)
     (->> seq
          ;; Divide into groups
          ;; The number of gaps are supposed to be the same on front and back
          (partition (count (remove-nils front-order)))
          ;; And group into front and back
          (partition 2)
          ;; Switch order in each group.
          ;; We have vectors with a front and back sequence.
          ;; For each map with the front and back order.
          (mapcat (fn [set]
                    (mapcat (fn [subseq order]
                              (map (fn [i]
                                     (if (nil? i)
                                       nil
                                       (nth subseq i)))
                                   order))
                            set
                            [front-order back-order])))))))

(defn- reverse-row-nil-order [row-order]
  "Helper for normalize-asym-group-layout"
  ;; Using the reverse order and the actual indexes of the order,
  ;; reverse the order preserving the index order.
  ;; I.e., for a row, front-order: [0 nil 1 nil nil]
  ;; is back-order:                [nil nil 0 nil 1]
  (loop [rev-o (reverse row-order)
         o-idxs (remove-nils row-order)
         res []]
    (if (empty? rev-o)
      res
      ;; When a value in the reverse order is nil, take nil.
      ;; If not nil, take the value from the original instead.
      (if (nil? (first rev-o))
        (recur (rest rev-o)
               o-idxs
               (conj res nil))
        (recur (rest rev-o)
               (rest o-idxs)
               (conj res (first o-idxs)))))))

(defn normalize-asym-group-layout [columns front-order seq]
  "Call normalize-group-layout with a layout that is the same on front and back
   but is assymetric. The order argument should describe the front."
  (let [rows (partition columns front-order)
        back-order (mapcat reverse-row-nil-order rows)]
    (normalize-group-layout [front-order back-order] seq)))

(defn separate [seq & idx-groups]
  "Return a list of vectors with values from seq.
   The first vector is values not filtered by indexes.
   The rest are values from the indexes in idx-groups.
   Used to take out specific cards out of a sequence."
  (let [idx-groups-normalized (map #(if (vector? %)
                                      %
                                      [%])
                                   idx-groups)
        group-idxs (range (count idx-groups))
        main (transient [])
        groups (repeatedly (count idx-groups)
                           #(transient []))]
    (doseq [i (range (count seq))]
      (let [v (nth seq i)
            in-grp-idx (some (fn [gi]
                               (some #(when (= i %) gi)
                                     (nth idx-groups-normalized gi)))
                             group-idxs)]
        (if (nil? in-grp-idx)
          (conj! main v)
          (conj! (nth groups in-grp-idx) v))))
    (concat [(persistent! main)]
            (map persistent! groups))))

;; TODO: maybe we should get the PDRectangle of the original
;; PDPage instead of using the image as point of departure.
;; This will be easier to handle for e.g. Sprawlopolis which
;; has several images in one page.
(defn optimal-assemble-parameters [[img-width img-height]
                                   [card-width card-height]]
  "Calculate margins and number of cards per page based on data
   from the original files")
