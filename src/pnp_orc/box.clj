(ns pnp-orc.box
  (:require [clojure.tools.logging :as log]
            [clojure.math.numeric-tower :as math]
            [mikera.image.core :as img]
            [pnp-orc.pdf :as pdf])
  (:import (org.apache.pdfbox.pdmodel.common PDRectangle)
           (org.apache.pdfbox.pdmodel PDDocument PDPage PDPageContentStream)
           (org.apache.pdfbox.pdmodel.graphics.image LosslessFactory PDImageXObject)
           (org.apache.pdfbox.util Matrix)
           (java.awt.geom AffineTransform)
           (java.awt Color)))

;; Points per mm is the PPI resolutions divided by the conversion constant.
(def points-per-mm (/ 72 25.4))
;; http://spencermortensen.com/articles/bezier-circle/
(def circle-k 0.551915024494)

(defn- ^Float mm->points [mm]
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

(defn- draw-rel-path [^PDPageContentStream content
                    [start-x start-y] & points]
  "Given and absolute start and relative points, draw path"
  (let [start-x-p (mm->points start-x)
        start-y-p (mm->points start-y)
        abs-points (rel-mm->abs-points [start-x-p start-y-p] points)]
    (.moveTo content start-x-p start-y-p)
    (doseq [p abs-points]
      (if-not (keyword? (first p))
        (.lineTo content (first p) (second p))
        (let [[[x1 y1] [x2 y2] [x3 y3]] (rest p)]
          (case (first p)
            :curve (do (.curveTo content x1 y1 x2 y2 x3 y3))))))))

(defn- stroke-rel-path [^PDPageContentStream content
                        start & points]
  "Given and absolute start and relative points, stroke path"
  (apply draw-rel-path content start points)
  (.stroke content))

(defn- fill-rel-path [^PDPageContentStream content
                      start & points]
  "Given and absolute start and relative points, stroke path"
  (apply draw-rel-path content start points)
  (.fill content))

(defn- stroke-lines [^PDPageContentStream content & lines]
  "Given pairs of absolute offset and relative end, stroke lines"
  (let [pairs (partition 2 lines)]
    (doseq [l pairs]
      (stroke-rel-path content (first l) (second l)))))

(defn- ^PDImageXObject load-image-with-ratio-check [^PDDocument doc
                                                    ratio-warning-threshold
                                                    path
                                                    rotation
                                                    w-h-ratio]
  (when path
    (let [img (img/load-image path)
          img-obj (LosslessFactory/createFromImage doc img)
          w (img/width img)
          h (img/height img)
          img-ratio (/ w h)
          w-h-ratio-with-rot (if (or (= rotation :up) (= rotation :down))
                               w-h-ratio
                               (/ 1 w-h-ratio))
          ratio-diff (math/abs (- img-ratio w-h-ratio-with-rot))]
      (if (< ratio-warning-threshold ratio-diff)
        (log/warn path "had width/height ratio that was" ratio-diff "off."
                  "Change image width to" (math/round (* h w-h-ratio-with-rot))
                  "or image height to" (math/round (/ w w-h-ratio-with-rot))
                  "to get closer to a ratio of" w-h-ratio-with-rot))
      img-obj)))

(defn- draw-image-if-not-nil [^PDPageContentStream content
                              ^PDImageXObject img
                              [x y] [w h]
                              orientation]
  (when img
    (let [wp (mm->points w)
          hp (mm->points h)
          [tx ty] (case orientation
                    :up [0 0]
                    :left [w 0]
                    :right [0 h]
                    :down [w h])
          q (case orientation :up 0, :left 1, :down 2, :right 3)
          xp (mm->points (+ x tx))
          yp (mm->points (+ y ty))
          ^AffineTransform transform (AffineTransform.)]
      (.translate transform xp yp)
      (.scale transform wp hp)
      (.quadrantRotate transform q)
      (.drawImage content img (Matrix. transform)))))

(defn make-card-box-pdf
  ([path width height depth]
   (make-card-box-pdf path width height depth {}))
  ([path
    width height depth
    {:keys [front-img-path back-img-path
            left-img-path right-img-path
            top-img-path bottom-img-path
            front-img-orient back-img-orient
            left-img-orient right-img-orient
            top-img-orient bottom-img-orient
            ratio-warning-threshold
            margin
            close-flap-straight-size-factor close-flap-curve-size-factor
            close-flap-ends-k-factor
            close-flap-middle-k-factor
            top-flaps-size-factor
            top-flap-slope-fold-side
            top-flap-slope-close-side
            glue-bottom-gap glue-bottom-slope
            glue-flaps-size-factor glue-flaps-slope
            glue-side-gap glue-side-slope
            close-hole-width
            close-hole-height                               ; width / 2 for half cicle
            close-hole-ends-k-factor                        ; circle-k for half circle
            close-hole-middle-k-factor                      ; circle-k for half circle
            line-width]
     :or   {front-img-orient                :up
            back-img-orient                 :up
            left-img-orient                 :right
            right-img-orient                :left
            top-img-orient                  :up
            bottom-img-orient               :up
            ratio-warning-threshold         0.01
            margin                          10
            close-flap-straight-size-factor 0.5
            close-flap-curve-size-factor    1.0
            close-flap-ends-k-factor        circle-k
            close-flap-middle-k-factor      circle-k
            top-flaps-size-factor           0.9
            top-flap-slope-fold-side        1
            top-flap-slope-close-side       3
            glue-bottom-gap                 2
            glue-bottom-slope               1
            glue-flaps-size-factor          0.85
            glue-flaps-slope                2
            glue-side-gap                   2
            glue-side-slope                 1
            close-hole-width                15
            close-hole-height               5
            close-hole-ends-k-factor        0.1
            close-hole-middle-k-factor      circle-k
            line-width                      0.01}}]
   "Create PDF with box for cards.
    All measures are in millimeters.
    An image may be a path or a vector with path and rotation:
    :left, :right or :rot for rotating left, right or 180 degrees."
   (let [width-height-ratio (/ width height)
         depth-height-ratio (/ depth height)
         width-depth-ratio (/ width depth)
         width-height-dim [width height]
         depth-height-dim [depth height]
         width-depth-dim [width depth]
         margin+depth (+ margin depth)
         -width (- width)
         -depth (- depth)
         -glue-side-slope (- glue-side-slope)
         -glue-flaps-slope (- glue-flaps-slope)
         -glue-bottom-slope (- glue-bottom-slope)
         half-width (/ width 2)
         close-flap-straight-size (* depth close-flap-straight-size-factor)
         close-flap-curve-size (* depth close-flap-curve-size-factor)
         close-flap-horiz-k (* close-flap-middle-k-factor half-width)
         close-flap-vert-k (* close-flap-ends-k-factor close-flap-curve-size)
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
         close-hole-horiz-k (* close-hole-middle-k-factor close-hole-half-width)
         close-hole-vert-k (* close-hole-ends-k-factor close-hole-height)
         close-hole-1 [:curve
                       [0 (- close-hole-vert-k)]
                       [(- close-hole-half-width close-hole-horiz-k)
                        (- close-hole-height)]
                       [close-hole-half-width (- close-hole-height)]]
         close-hole-2 [:curve
                       [close-hole-horiz-k 0]
                       [close-hole-half-width
                        (- close-hole-height close-hole-vert-k)]
                       [close-hole-half-width close-hole-height]]
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
       (.addPage doc page) height
       (with-open [^PDPageContentStream content (PDPageContentStream. doc page)]
         (let [load-img (partial load-image-with-ratio-check doc
                                 ratio-warning-threshold)
               draw-img (partial draw-image-if-not-nil content)
               front-img (load-img front-img-path front-img-orient
                                   width-height-ratio)
               back-img (load-img back-img-path back-img-orient
                                  width-height-ratio)
               left-img (load-img left-img-path left-img-orient
                                  depth-height-ratio)
               right-img (load-img right-img-path right-img-orient
                                   depth-height-ratio)
               top-img (load-img top-img-path top-img-orient
                                 width-depth-ratio)
               bottom-img (load-img bottom-img-path bottom-img-orient
                                    width-depth-ratio)]
           (draw-img front-img
                     [margin+depth margin+depth]
                     width-height-dim
                     front-img-orient)
           (draw-img back-img
                     [(+ margin width (* 2 depth)) margin+depth]
                     width-height-dim
                     back-img-orient)
           (draw-img left-img
                     [margin margin+depth]
                     depth-height-dim
                     left-img-orient)
           (draw-img right-img
                     [(+ margin+depth width) margin+depth]
                     depth-height-dim
                     right-img-orient)
           (draw-img top-img
                     [margin+depth (+ margin+depth height)]
                     width-depth-dim
                     top-img-orient)
           (draw-img bottom-img
                     [margin+depth margin]
                     width-depth-dim
                     bottom-img-orient)
           (.setNonStrokingColor content Color/white)
           (fill-rel-path content
                          [(+ margin width (* 2 depth) close-edge-part-size)
                           (+ margin+depth height)]
                          close-hole-1 close-hole-2)
           (.setStrokingColor content Color/black)
           (.setLineWidth content (mm->points line-width))
           (stroke-rel-path content [margin margin+depth]
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
                             [0 close-flap-vert-k]
                             [(- half-width close-flap-horiz-k)
                              close-flap-curve-size]
                             [half-width close-flap-curve-size]]
                            [:curve
                             [close-flap-horiz-k 0]
                             [half-width
                              (- close-flap-vert-k close-flap-curve-size)]
                             [half-width (- close-flap-curve-size)]]
                            [0 (- close-flap-straight-size)]
                            [0 (- depth)]
                            ;; top right flap
                            [top-flap-slope-fold-side top-flaps-size]
                            [top-flaps-edge-size 0]
                            [top-flap-slope-close-side (- top-flaps-size)]
                            ;; close edge - circles are one arc at a time.
                            [close-edge-part-size 0]
                            close-hole-1
                            close-hole-2
                            [close-edge-part-size 0]
                            ;; glue side
                            [glue-side-size -glue-side-slope]
                            [0 (- (* 2 glue-side-slope) height)]
                            [-glue-side-size -glue-side-slope]
                            ;; glue bottom
                            [-glue-bottom-slope -glue-bottom-size]
                            [-glue-bottom-edge-size 0]
                            [-glue-bottom-slope glue-bottom-size]
                            ;; right glue flap
                            [-glue-flaps-slope -glue-flaps-size]
                            [-glue-flaps-edge-size 0]
                            [-glue-flaps-slope glue-flaps-size]
                            ;; bottom
                            [0 -depth]
                            [-width 0]
                            [0 depth]
                            ;; left glue flap
                            [-glue-flaps-slope -glue-flaps-size]
                            [-glue-flaps-edge-size 0]
                            [-glue-flaps-slope glue-flaps-size])
           ;; Dotted lines.
           (.setLineDashPattern content (float-array [0.7 10.0]) (float 1.0))
           (stroke-lines
             content
             ;; top
             [margin margin+depth] [(* 2 (+ depth width)) 0]
             ;; bottom
             [margin (+ margin depth height)] [(+ width (* 2 depth)) 0]
             ;; lid
             [margin+depth (+ margin height (* 2 depth))] [width 0]
             ;; vertical lines from left to right
             [margin+depth margin+depth] [0 height]
             [(+ margin depth width) margin+depth] [0 height]
             [(+ margin width (* 2 depth)) margin+depth] [0 height]
             [(+ margin (* 2 width) (* 2 depth)) margin+depth] [0 height])))))))

(defn make-poker-card-box-pdf
  "Make box for poker size cards (2.5 x 3.5 in / 63.5 x 88.9 mm)"
  ([path depth]
   (make-poker-card-box-pdf path depth 0 0 {}))
  ([path depth options]
   (make-poker-card-box-pdf path depth 0 0 options))
  ([path depth width-add height-add]
    (make-poker-card-box-pdf path depth width-add height-add {}))
  ([path depth width-add height-add options]
   (let [width (+ 63.5 width-add)
         height (+ 88.9 height-add)]
     (make-card-box-pdf path width height depth options))))
