(ns pnp-proc.tools
  "Functions for preparing recipies"
  (:require [mikera.image.core  :as img]
            [pnp-proc.file      :as file]
            [pnp-proc.pdf       :as pdf])
  (:import
    (org.apache.pdfbox.contentstream.operator DrawObject)
    (org.apache.pdfbox.contentstream.operator.state Concatenate
                                                    Restore
                                                    Save
                                                    SetGraphicsStateParameters
                                                    SetMatrix)
    (org.apache.pdfbox.pdmodel PDDocument PDResources)
    (org.apache.pdfbox.pdmodel.graphics.image PDImageXObject)
    (org.apache.pdfbox.pdmodel.graphics.form PDFormXObject)
    (org.apache.pdfbox.contentstream PDFStreamEngine)
    (org.apache.pdfbox.rendering PDFRenderer)
    (org.apache.pdfbox.util Matrix)))

(defn dump-images-from-pdf [pdf folder]
  "Dump all images from a PDF."
  (let [images (pdf/get-images-from-pdf pdf)]
    (file/save-images
      images (str (clojure.java.io/file folder "img-%03d.png")))))

;;; The method to get DPI can also be used to find the correct scale
;;; and is therefore generalized.
(defn- get-bitmap-attribute
  "Find an attribute for an image on a page that is only accessible during
   rendering.
   The function f should have the signature:
     [^PDImageXObject obj ^Matrix matrix].
   Based on org.apache.pdfbox.examples.util.PrintImageLocations."
  ([pdf page-idx image-idx f]
   (pdf/with-open-doc [doc pdf]
     (let [page (.getPage doc page-idx)]
       (get-bitmap-attribute page image-idx f))))
  ([page image-idx f]
   (let [cur-img-idx (atom -1)
         value (atom false)
         attr-finder
         (proxy [PDFStreamEngine] []
           (processOperator [operator operands]
             (if (= "Do" (.getName operator))
               (let [obj (-> ^PDResources (.getResources this)
                             (.getXObject (first operands)))]
                 (cond
                   (instance? PDImageXObject obj)
                   (do
                     (swap! cur-img-idx inc)
                     (let [matrix (-> this .getGraphicsState
                                      .getCurrentTransformationMatrix)
                           new-v (f obj matrix)]
                       (if (= @cur-img-idx image-idx)
                         (reset! value new-v))))
                   ;; Images may be embedded in forms.
                   (instance? PDFormXObject obj)
                   (.showForm this obj)))
               (proxy-super processOperator operator operands))))]
     ;; Yes, they are all necessary (cf. constructor in PrintImageLocations).
     (.addOperator attr-finder (Concatenate.))
     (.addOperator attr-finder (DrawObject.))
     (.addOperator attr-finder (SetGraphicsStateParameters.))
     (.addOperator attr-finder (Save.))
     (.addOperator attr-finder (Restore.))
     (.addOperator attr-finder (SetMatrix.))
     ;; Get the value.
     (.processPage attr-finder page)
     @value)))

(defn- dpi-fn [^PDImageXObject img ^Matrix matrix]
  "Helper for get-bitmap-dpi."
  ;; The width in pixels divided by the width in inches
  ;; gives the DPI to render by to match the bitmap.
  ;; 72 DPI must be the default rendering for PDF's, it seems.
  (/ (.getWidth img)
     ;; Scaling factor is negative if the image is rotated.
     (/ (Math/abs (.getScalingFactorX matrix))
        72)))

(defn- get-bitmap-dpi
  "Find the correct DPI for dumping a page as image so that bitmaps
   come out with their original dimensions.
   Based on org.apache.pdfbox.examples.util.PrintImageLocations."
  ([page image-idx]
   (get-bitmap-attribute page image-idx dpi-fn))
  ;; For debugging.
  ([pdf page-idx image-idx]
   (get-bitmap-attribute pdf page-idx image-idx dpi-fn)))

(defn get-bitmap-scale [pdf page-idx image-idx]
  "Find the scale of an image on a page.
   Used for producing the resulting PDF."
  (get-bitmap-attribute pdf page-idx image-idx
                        (fn [^PDImageXObject img ^Matrix matrix]
                          ;; The scaling factor is the size in points.
                          ;; Scaling factor is negative if the image is rotated.
                          (/ (Math/abs (.getScalingFactorX matrix))
                             (.getWidth img)))))


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
