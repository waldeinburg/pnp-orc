(ns pnp-proc.pdf
  (:require pdfboxing.common)
  (:import org.apache.pdfbox.pdmodel.PDDocument
           org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject
           org.apache.pdfbox.contentstream.PDContentStream))

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

(defmacro with-open-doc [binding & body]
  (let [name (first binding)
        pdf (second binding)]
    `(with-open [~name
                 (pdfboxing.common/obtain-document ~pdf)]
       ~@body)))

(defn get-images-from-pdf
  "Get images from pdf.
   Optionally specify a sequence of page indexes.
   The sequence is realized and the document is closed."
  ([pdf]
   (get-images-from-pdf pdf []))
  ([pdf page-idxs]
   (with-open-doc [doc pdf]
     (let [all-pages (.getPages doc)
           pages (if (empty? page-idxs)
                   all-pages
                   (map (partial nth all-pages) page-idxs))]
       (doall ; Or else the document might be closed when reading an image.
        (mapcat get-images-from-content-stream
                pages))))))
