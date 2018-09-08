(ns pnp-proc.pdf
  (:require pdfboxing.common)
  (:import (org.apache.pdfbox.pdmodel.graphics.image LosslessFactory
                                                     PDImageXObject)
           (org.apache.pdfbox.contentstream PDContentStream)
           (org.apache.pdfbox.pdmodel PDDocument
                                      PDPage
                                      PDPageContentStream)
           (org.apache.pdfbox.pdmodel.common PDRectangle)))

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

(defn images->pdf
  ([path images scale]
   (with-make-pdf [doc path]
     (add-images-as-pages! doc images scale))))
