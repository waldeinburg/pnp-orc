(ns pnp-proc.dump
  "Functions for dumping data for preparing recipies"
  (:require [mikera.image.core :as img]
            [pnp-proc.worker   :as worker]
            [pnp-proc.pdf      :as pdf])
  (:import org.apache.pdfbox.contentstream.operator.DrawObject
           [org.apache.pdfbox.contentstream.operator.state
            Concatenate Restore Save SetGraphicsStateParameters SetMatrix]
           org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
           org.apache.pdfbox.rendering.PDFRenderer))

(defn dump-images-from-pdf [pdf folder]
  "Dump all images from a PDF.
   For preparing recipies."
  (let [images (pdf/get-images-from-pdf pdf)]
    (worker/save-images
     images (str (clojure.java.io/file folder "img-%03d.png")))))

(defn- get-bitmap-dpi [page]
  "Find the correct DPI for dumping a page as image so that bitmaps
   come out with their original dimensions.
   Based on org.apache.pdfbox.examples.util.PrintImageLocations"
  (let [dpi (atom false)
        dpi-finder
        (proxy [org.apache.pdfbox.contentstream.PDFStreamEngine] []
          (processOperator [operator operands]
            (if (= "Do" (.getName operator))
              (let [obj (-> (.getResources this)
                            (.getXObject (first operands)))]
                (if (instance? PDImageXObject obj)
                  (let [matrix (-> this .getGraphicsState
                                   .getCurrentTransformationMatrix)
                        ;; The width in pixels divided by the width in inches
                        ;; gives the DPI to render by to match the bitmap.
                        ;; 72 DPI must be the default rendering for PDF's, it seems.
                        new-dpi (/ (.getWidth obj)
                                   (/ (.getScalingFactorX matrix) 72))]
                    ;; Use the first value found.
                    (swap! dpi #(or % %2) new-dpi))))
              (proxy-super processOperator operator operands))))]
    ;; Yes, they are all necessary (cf. constructor in PrintImageLocations).
    (.addOperator dpi-finder (Concatenate.))
    (.addOperator dpi-finder (DrawObject.))
    (.addOperator dpi-finder (SetGraphicsStateParameters.))
    (.addOperator dpi-finder (Save.))
    (.addOperator dpi-finder (Restore.))
    (.addOperator dpi-finder (SetMatrix.))
    ;; Get the value.
    (.processPage dpi-finder page)
    @dpi))

(defn render-page-to-image
  "Render a page to an image file.
   Useful for determining coordinates based on cut lines for PDF's
   containing several images per page."
  ([pdf page-idx file]
   (pdf/with-open-doc [doc pdf]
     (let [renderer (PDFRenderer. doc)
           page (.getPage doc page-idx)
           dpi (get-bitmap-dpi page)
           image (.renderImageWithDPI renderer page-idx dpi)]
       (img/save image file)))))
