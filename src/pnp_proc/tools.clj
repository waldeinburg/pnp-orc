(ns pnp-proc.tools
  "Functions for preparing recipies"
  (:require [mikera.image.core  :as img]
            [pnp-proc.file      :as file]
            [pnp-proc.pdf       :as pdf])
  (:import org.apache.pdfbox.contentstream.operator.DrawObject
           [org.apache.pdfbox.contentstream.operator.state
            Concatenate Restore Save SetGraphicsStateParameters SetMatrix]
           org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
           org.apache.pdfbox.rendering.PDFRenderer
           org.apache.pdfbox.contentstream.PDFStreamEngine
           (org.apache.pdfbox.pdmodel PDDocument)))

(defn dump-images-from-pdf [pdf folder]
  "Dump all images from a PDF."
  (let [images (pdf/get-images-from-pdf pdf)]
    (file/save-images
     images (str (clojure.java.io/file folder "img-%03d.png")))))

(defn- get-bitmap-dpi [page image-idx]
  "Find the correct DPI for dumping a page as image so that bitmaps
   come out with their original dimensions.
   Based on org.apache.pdfbox.examples.util.PrintImageLocations"
  (let [cur-img-idx (atom -1)
        dpi (atom false)
        dpi-finder
        (proxy [PDFStreamEngine] []
          (processOperator [operator operands]
            (if (= "Do" (.getName operator))
              (let [^PDImageXObject obj (-> (.getResources this)
                                            (.getXObject (first operands)))]
                (if (instance? PDImageXObject obj)
                  (swap! cur-img-idx inc)
                  (let [matrix (-> this .getGraphicsState
                                   .getCurrentTransformationMatrix)
                        ;; The width in pixels divided by the width in inches
                        ;; gives the DPI to render by to match the bitmap.
                        ;; 72 DPI must be the default rendering for PDF's, it seems.
                        new-dpi (/ (.getWidth obj)
                                   (/ (.getScalingFactorX matrix) 72))]
                    (if (= @cur-img-idx image-idx)
                      (reset! dpi new-dpi)))))
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
  ([pdf page-idx image-idx file]
   (pdf/with-open-doc [^PDDocument doc pdf]
     (let [renderer (PDFRenderer. doc)
           page (.getPage doc page-idx)
           dpi (get-bitmap-dpi page image-idx)
           image (.renderImageWithDPI renderer page-idx dpi)]
       (img/save image file)))))

(defn get-image-sizes [images]
  "Get a vector of vectors with image width and height.
   For inspecting anormalies found by images-equal-size?"
  (map (fn [img]
         [(img/width img) (img/height img)])
       images))

(defn images-equal-size? [images]
  "Are all images in the sequence of equal size?
   Use together with pdf/get-images-from-pdf"
  (let [first-img (first images)
        width (img/width first-img)
        height (img/width first-img)]
    (nil? (some #((or (not= width (img/width %))
                      (not= height (img/height %))))
                (rest images)))))
