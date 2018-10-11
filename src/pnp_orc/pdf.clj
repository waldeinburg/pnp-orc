(ns pnp-orc.pdf
  (:require pdfboxing.common
            pdfboxing.info)
  (:import (org.apache.pdfbox.pdmodel.graphics.image LosslessFactory
                                                     PDImageXObject)
           (org.apache.pdfbox.pdmodel.graphics.form PDFormXObject)
           (org.apache.pdfbox.contentstream PDContentStream
                                            PDFStreamEngine)
           (org.apache.pdfbox.contentstream.operator DrawObject)
           (org.apache.pdfbox.contentstream.operator.state Concatenate
                                                           Restore
                                                           Save
                                                           SetGraphicsStateParameters
                                                           SetMatrix)
           (org.apache.pdfbox.pdmodel PDDocument
                                      PDPage
                                      PDPageContentStream
                                      PDResources)
           (org.apache.pdfbox.pdmodel.common PDRectangle)
           (org.apache.pdfbox.rendering PDFRenderer)
           (org.apache.pdfbox.util Matrix)))

(defn- get-objects [^PDContentStream cs]
  "Take an object that can return a PDResources object
   and get XObject objects"
  (let [res (.getResources cs)]
    (map #(.getXObject res %)
         (.getXObjectNames res))))

(defn get-images-from-content-stream [^PDContentStream cs]
  "Get images as a lazy sequence from a content stream, e.g., a page"
  (mapcat #(if (instance? PDImageXObject %)
             [(.getImage %)]
             (if (instance? PDContentStream %)
               (get-images-from-content-stream %)))
          (get-objects cs)))

(defmacro with-open-doc [bindings & body]
  "As with-open but with the symbols being bound to a pdf document"
  (let [doc-bindings
        (into []
              (map-indexed #(if (odd? %1)
                              `(pdfboxing.common/obtain-document ~%2)
                              %2)
                           bindings))]
    `(with-open ~doc-bindings
       ~@body)))

(defmacro with-make-pdf [bindings & body]
  "As with-open-doc but with binding being to paths to be saved"
  (let [bind-pairs (partition 2 bindings)
        doc-bindings (into []
                           (mapcat #(list (first %) '(PDDocument.))
                                   bind-pairs))
        saves (map (fn [b]
                     `(.save ^PDDocument ~(first b) ^String ~(second b)))
                   bind-pairs)]
    `(with-open ~doc-bindings
       ~@body
       ~@saves)))

(defn get-images-from-pdf
  "Get images from pdf.
   Optionally specify a sequence of page indexes.
   The sequence is realized and the document is closed."
  ([pdf]
   (get-images-from-pdf pdf []))
  ([pdf page-idxs]
   (with-open-doc [^PDDocument doc pdf]
     (let [all-pages (.getPages doc)
           pages (if (empty? page-idxs)
                   all-pages
                   (map (partial nth all-pages) page-idxs))]
       (doall ; Or else the document might be closed when reading an image.
         (mapcat get-images-from-content-stream
                 pages))))))

(defn add-image-as-page! [^PDDocument doc image scale]
  (let [^PDImageXObject pd-img (LosslessFactory/createFromImage doc image)
        ^float img-width (* (.getWidth pd-img) scale)
        ^float img-height (* (.getHeight pd-img) scale)
        rect (PDRectangle. img-width img-height)
        page (PDPage. rect)
        x (float 0)
        y (float 0)]
    (.addPage doc page)
    (with-open [^PDPageContentStream content (PDPageContentStream. doc page)]
      (.drawImage content pd-img
                  x y
                  img-width img-height))))

(defn add-images-as-pages! [doc images scale]
  (doseq [img images]
    (add-image-as-page! doc img scale)))

(defn add-page-from! [^PDDocument dest-doc
                      ^PDDocument src-doc
                      page-idx]
  "Copy a page from one document to another.
   The the destination document must be closed before the source."
  (.addPage dest-doc (.getPage src-doc page-idx)))

(defn images->pdf
  ([path images scale]
   (with-make-pdf [doc path]
     (add-images-as-pages! doc images scale))))

;;; The method to get DPI can also be used to find the correct scale
;;; and is therefore generalized.
(defn- get-bitmap-attribute
  "Find an attribute for an image on a page that is only accessible during
   rendering.
   The function f should have the signature:
     [^PDImageXObject obj ^Matrix matrix].
   Based on org.apache.pdfbox.examples.util.PrintImageLocations."
  ([pdf page-idx image-idx f]
   (with-open-doc [doc pdf]
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

(defn get-bitmap-dpi [pdf page-idx image-idx]
  "Find the correct DPI for dumping a page as image so that bitmaps
   come out with their original dimensions.
   Based on org.apache.pdfbox.examples.util.PrintImageLocations."
  (get-bitmap-attribute pdf page-idx image-idx dpi-fn))

(defn get-bitmap-scale [pdf page-idx image-idx]
  "Find the scale of an image on a page.
   Used for producing the resulting PDF."
  (get-bitmap-attribute pdf page-idx image-idx
                        (fn [^PDImageXObject img ^Matrix matrix]
                          ;; The scaling factor is the size in points.
                          ;; Scaling factor is negative if the image is rotated.
                          (/ (Math/abs (.getScalingFactorX matrix))
                             (.getWidth img)))))

(defn render-pages-to-images [pdf dpi & page-idxs]
  (with-open-doc [^PDDocument doc pdf]
    (let [renderer (PDFRenderer. doc)]
      ;; Not lazy (doall) because doc will close.
      (doall
        (map #(.renderImageWithDPI renderer % dpi)
             (if (empty? page-idxs)
               (range (pdfboxing.info/page-number doc))
               page-idxs))))))

(defn render-page-to-image
  ([pdf dpi page-idx]
   (first (render-pages-to-images pdf dpi page-idx))))
