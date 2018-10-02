(ns pnp-proc.box
  (:require [clojure.tools.logging :as log]
            [clojure.math.numeric-tower :as math]
            [mikera.image.core :as img]
            [pnp-proc.pdf :as pdf])
  (:import (org.apache.pdfbox.pdmodel.common PDRectangle)
           (org.apache.pdfbox.pdmodel PDDocument PDPage PDPageContentStream)))

;; Points per mm is the PPI resolutions divided by the conversion constant.
(def points-per-mm (/ 72 25.4))
;; http://spencermortensen.com/articles/bezier-circle/
(def circle-k 0.551915024494)

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
                        [start-x start-y] & points]
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

(defn- load-image-with-ratio-check [path w-h-ratio ratio-warning-threshold]
  (let [img (img/load-image path)
        w (img/width img)
        h (img/height img)
        img-ratio (/ w h)
        ratio-diff (- img-ratio w-h-ratio)]
    (if (< ratio-warning-threshold ratio-diff)
      (log/warn path "had width/height ratio that was" ratio-diff "off."
                "Change image width to" (math/round (* h w-h-ratio))
                "or image height to" (math/round (/ w w-h-ratio))
                "to get closer to a ratio of" w-h-ratio))
    img))

(defn make-card-box-pdf
  ([path width height depth]
   (make-card-box-pdf path width height depth {}))
  ([path
    width height depth
    {:keys [front-img-path back-img-path
            left-img-path right-img-path
            top-img-path bottom-img-path
            ratio-warning-threshold
            margin
            close-flap-straight-size-factor close-flap-curve-size-factor
            top-flaps-size-factor
            glue-bottom-gap glue-bottom-slope
            glue-flaps-size-factor glue-flaps-slope
            glue-side-gap glue-side-slope
            top-flap-slope-fold-side
            top-flap-slope-close-side
            close-hole-width
            close-hole-height                               ; width / 2 for cicle
            close-hole-ends-k-factor                        ; circle-k for circle
            close-hole-middle-k-factor                      ; circle-k for circle
            line-width]
     :or   {ratio-warning-threshold         0.005
            margin                          10
            close-flap-straight-size-factor 0.7
            close-flap-curve-size-factor    1.2
            top-flaps-size-factor           0.9
            glue-bottom-gap                 2
            glue-bottom-slope               1
            glue-flaps-size-factor          0.85
            glue-flaps-slope                2
            glue-side-gap                   2
            glue-side-slope                 1
            top-flap-slope-fold-side        1
            top-flap-slope-close-side       3
            close-hole-width                15
            close-hole-height               5
            close-hole-ends-k-factor        circle-k
            close-hole-middle-k-factor      0.1
            line-width                      0.2}}]
   "Create PDF with box for cards.
    All measures are in millimeters.
    An image may be a path or a vector with path and rotation:
    :left, :right or :rot for rotating left, right or 180 degrees."
   (let [width-height-ratio (/ width height)
         depth-height-ratio (/ depth height)
         width-depth-ratio (/ width depth)
         load-img #(if %1
                     (load-image-with-ratio-check %1 %2 ratio-warning-threshold))
         front-img (load-img front-img-path width-height-ratio)
         back-img (load-img back-img-path width-height-ratio)
         left-img (load-img left-img-path depth-height-ratio)
         right-img (load-img right-img-path depth-height-ratio)
         top-img (load-img top-img-path width-depth-ratio)
         bottom-img (load-img bottom-img-path width-depth-ratio)
         -width (- width)
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
         close-edge-part-size (/ (- width close-hole-width) 2)
         close-hole-half-width (/ close-hole-width 2)
         close-hole-horiz-k (* close-hole-ends-k-factor close-hole-half-width)
         close-hole-vert-k (* close-hole-middle-k-factor close-hole-height)
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
                           [0 (- close-hole-vert-k)]
                           [(- close-hole-half-width close-hole-horiz-k)
                            (- close-hole-height)]
                           [close-hole-half-width (- close-hole-height)]]
                          [:curve
                           [close-hole-horiz-k 0]
                           [close-hole-half-width
                            (- close-hole-height close-hole-vert-k)]
                           [close-hole-half-width close-hole-height]]
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

(defn make-poker-card-box-pdf
  "Make box for poker size cards (63.5mm x 88.9mm)"
  ([path depth]
    (make-poker-card-box-pdf path depth {}))
  ([path depth options]
    (make-card-box-pdf path 64.5 89.9 depth options)))
