(ns pnp-proc.box
  (:require [pnp-proc.pdf :as pdf])
  (:import (org.apache.pdfbox.pdmodel.common PDRectangle)
           (org.apache.pdfbox.pdmodel PDDocument PDPage PDPageContentStream)))

;; Points per mm is the PPI resolutions divided by the conversion constant.
(def points-per-mm (/ 72 25.4))
;; http://spencermortensen.com/articles/bezier-circle/
;; P_0 = (0,1)
;,
;P_1 = (c,1)
;,
;P_2 = (1,c)
;,
;P_3 = (1,0
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

(defn- draw-and-get-position [^PDPageContentStream content [cur-x cur-y] point]
  (if-not (keyword? (first point))
    (do (.lineTo content (first point) (second point))
        point)
    (let [[[x1 y1 :as p1] [x2 y2 :as p2] [x3 y3 :as p3]] (rest point)]
      (case (first point)
        :curve (do (.curveTo content x1 y1 x2 y2 x3 y3)
                   p3)
        :half-circle-down (do p1)))))

(defn- stroke-rel-path [^PDPageContentStream content
                        [start-x start-y :as start] & points]
  (let [start-x-p (mm->points start-x)
        start-y-p (mm->points start-y)
        abs-points (rel-mm->abs-points [start-x-p start-y-p] points)]
    (.moveTo content start-x-p start-y-p)
    (loop [cur-pos start
           rest-points abs-points]
      (when-not (empty? rest-points)
        (let [p (first rest-points)
              new-cur-pos (draw-and-get-position content cur-pos p)]
          (recur new-cur-pos (rest rest-points))))))
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
            close-flap-min-size
            slope
            line-width]
     :or   {margin              10
            close-flap-min-size 15
            slope               1
            line-width          0.2}}]
   "Create PDF with box for cards.
    All measures are in millimeters.
    An image may be a path or a vector with path and rotation:
    :left, :right or :rot for rotating left, right or 180 degrees."
   (let [close-flap-size (max (* depth 1.5) close-flap-min-size)
         glue-bottom-size (- depth 1)
         glue-flaps-size glue-bottom-size
         top-flaps-size depth
         width-total (+ (* 2 margin)
                        (* 2 width)
                        (* 3 depth))
         height-total (+ (* 2 margin)
                         close-flap-size
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
                          [slope top-flaps-size]
                          [(- depth (* 2 slope)) 0]
                          [slope (- top-flaps-size)]
                          ;; top
                          [0 depth]
                          [:curve
                           [0 close-flap-size]
                           [width close-flap-size]
                           [width 0]])
         ;; Dotted lines.
         ;(.setLineDashPattern content [1 4] 1)
         )))))
