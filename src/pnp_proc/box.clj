(ns pnp-proc.box
  (:require [pnp-proc.pdf :as pdf])
  (:import (org.apache.pdfbox.pdmodel.common PDRectangle)
           (org.apache.pdfbox.pdmodel PDDocument PDPage PDPageContentStream)))

;; Points per mm is the PPI resolutions divided by the conversion constant.
(def points-per-mm (/ 72 25.4))
;; http://spencermortensen.com/articles/bezier-circle/
(def c 0.551915024494)

(defn- mm->points [mm]
  (* mm points-per-mm))

(defn- rel-mm->abs-points [start points]
  (loop [[offset-x offset-y] start
         rest-points points
         result (transient [])]
    (if (empty? rest-points)
      (persistent! result)
      (let [cur (first rest-points)
            ;; Special form, [:curve p1 p2 p3 p4], or single point?
            is-special (keyword? (first cur))
            ;; Relative points.
            rel-ps (if is-special
                     (rest cur)
                     (list cur))
            ;; Convert to absolute points.
            abs-ps (map (fn [[x y]]
                          [(+ offset-x (mm->points x))
                           (+ offset-y (mm->points y))])
                        rel-ps)
            ;; The next offset is the last point or the single point.
            next-offset (last abs-ps)
            ;; Join with keyword or unwrap single point.
            abs (if is-special
                  (cons (first cur) abs-ps)
                  (first abs-ps))]
        (recur next-offset (rest rest-points) (conj! result abs))))))

(defn- stroke-rel-path [^PDPageContentStream content
                        [start-x start-y :as start] & points]
  (let [start-x-p (mm->points start-x)
        start-y-p (mm->points start-y)
        abs-points (rel-mm->abs-points [start-x-p start-y-p] points)]
    (.moveTo content start-x-p start-y-p)
    (doseq [p abs-points]
      (if-not (keyword? (first p))
        (.lineTo content (first p) (second p))
        (let [[[x1 y1] [x2 y2] [x3 y3]] (rest p)]
          (case (first p)
            :curve (do (.curveTo content x1 y1 x2 y2 x3 y3)))))))
  (.stroke content))

(defn make-box-pdf
  ([path width height depth]
   (make-box-pdf path width height depth {}))
  ([path
    width height depth
    {:keys [front-img back-img
            right-img left-img
            top-img bottom-img
            margin
            close-flap-straight-size-factor close-flap-curve-size-factor
            top-flaps-size-factor
            glue-bottom-gap glue-bottom-slope
            glue-flaps-size-factor glue-flaps-slope
            glue-side-gap glue-side-slope
            top-flap-slope-fold-side
            top-flap-slope-close-side
            close-hole-diameter
            line-width]
     :or   {margin                          10
            close-flap-straight-size-factor 0.3
            close-flap-curve-size-factor    0.7
            top-flaps-size-factor           0.9
            glue-bottom-gap                 2
            glue-bottom-slope               1
            glue-flaps-size-factor          0.85
            glue-flaps-slope                2
            glue-side-gap                   2
            glue-side-slope                 1
            top-flap-slope-fold-side        1
            top-flap-slope-close-side       3
            close-hole-diameter             10
            line-width                      0.2}}]
   "Create PDF with box for cards.
    All measures are in millimeters.
    An image may be a path or a vector with path and rotation:
    :left, :right or :rot for rotating left, right or 180 degrees."
   (let [-width (- width)
         -depth (- depth)
         -glue-side-slope (- glue-side-slope)
         -glue-flaps-slope (- glue-flaps-slope)
         -glue-bottom-slope (- glue-bottom-slope)
         close-flap-straight-size (* depth close-flap-straight-size-factor)
         close-flap-curve-size (* depth close-flap-curve-size-factor)
         glue-bottom-size (- depth glue-bottom-gap)
         -glue-bottom-size (- glue-bottom-size)
         glue-bottom-edge-size (- width (* 2 glue-bottom-slope))
         -glue-bottom-edge-size (- glue-bottom-edge-size)
         glue-flaps-size (* glue-bottom-size glue-flaps-size-factor)
         -glue-flaps-size (- glue-flaps-size)
         glue-flaps-edge-size (- depth (* 2 glue-flaps-slope))
         -glue-flaps-edge-size (- glue-flaps-edge-size)
         top-flaps-size (* depth top-flaps-size-factor)
         top-flaps-edge-size (- depth
                                top-flap-slope-fold-side
                                top-flap-slope-close-side)
         close-edge-part-size (/ (- width close-hole-diameter) 2)
         close-hole-radius (/ close-hole-diameter 2)
         close-hole-k (* c close-hole-radius)
         glue-side-size (- depth glue-side-gap)
         -glue-side-size (- glue-side-size)
         width-total (+ (* 2 margin)
                        (* 2 width)
                        (* 3 depth))
         height-total (+ (* 2 margin)
                         close-flap-straight-size
                         close-flap-curve-size
                         height
                         (* 2 depth))
         rect (PDRectangle. (mm->points width-total)
                            (mm->points height-total))
         page (PDPage. rect)]
     (pdf/with-make-pdf [^PDDocument doc path]
       (.addPage doc page)
       (with-open [^PDPageContentStream content (PDPageContentStream. doc page)]
         (.setStrokingColor content 0 0 0)
         (.setLineWidth content (mm->points line-width))
         (stroke-rel-path content [margin (+ margin depth)]
                          ;; left side
                          [0 height]
                          ;; top left flap
                          [top-flap-slope-close-side top-flaps-size]
                          [top-flaps-edge-size 0]
                          [top-flap-slope-fold-side (- top-flaps-size)]
                          ;; top
                          [0 depth]
                          [0 close-flap-straight-size]
                          [:curve
                           [0 close-flap-curve-size]
                           [width close-flap-curve-size]
                           [width 0]]
                          [0 (- close-flap-straight-size)]
                          [0 (- depth)]
                          ;; top right flap
                          [top-flap-slope-fold-side top-flaps-size]
                          [top-flaps-edge-size 0]
                          [top-flap-slope-close-side (- top-flaps-size)]
                          ;; close edge - circles are one arc at a time.
                          [close-edge-part-size 0]
                          [:curve
                           [0 (- close-hole-k)]
                           [(- close-hole-radius close-hole-k)
                            (- close-hole-radius)]
                           [close-hole-radius (- close-hole-radius)]]
                          [:curve
                           [close-hole-k 0]
                           [close-hole-radius close-hole-k]
                           [close-hole-radius close-hole-radius]]
                          [close-edge-part-size 0]
                          ;; glue side
                          [glue-side-size -glue-side-slope]
                          [0 (- (* 2 glue-side-slope) height)]
                          [-glue-side-size -glue-side-slope]
                          ;; bottom
                          [0 -depth]
                          [-width 0]
                          [0 depth]
                          ;; right glue flap
                          [-glue-flaps-slope -glue-flaps-size]
                          [-glue-flaps-edge-size 0]
                          [-glue-flaps-slope glue-flaps-size]
                          ;; glue bottom
                          [-glue-bottom-slope -glue-bottom-size]
                          [-glue-bottom-edge-size 0]
                          [-glue-bottom-slope glue-bottom-size]
                          ;; left glue flap
                          [-glue-flaps-slope -glue-flaps-size]
                          [-glue-flaps-edge-size 0]
                          [-glue-flaps-slope glue-flaps-size])
         ;; Dotted lines.
         (.setLineDashPattern content (float-array [0.7 10.0]) (float 1.0))
         ;; top
         (stroke-rel-path content
                          [margin (+ margin depth)]
                          [(* 2 (+ depth width)) 0])
         ;; bottom
         (stroke-rel-path content
                          [margin (+ margin depth height)]
                          [(+ width (* 2 depth)) 0])
         ;; lid
         (stroke-rel-path content
                          [(+ margin depth) (+ margin height (* 2 depth))]
                          [width 0])
         ;; vertical lines from left to right
         (stroke-rel-path content
                          [(+ margin depth) (+ margin depth)]
                          [0 height])
         (stroke-rel-path content
                          [(+ margin depth width) (+ margin depth)]
                          [0 height])
         (stroke-rel-path content
                          [(+ margin width (* 2 depth)) (+ margin depth)]
                          [0 height])
         (stroke-rel-path content
                          [(+ margin (* 2 width) (* 2 depth)) (+ margin depth)]
                          [0 height]))))))
