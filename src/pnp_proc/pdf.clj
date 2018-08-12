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

(defn- get-images-from-content-stream [^PDContentStream cs]
  (mapcat #(if (instance? PDImageXObject %)
             [(.getImage %)]
             (if (instance? PDContentStream %)
               (get-images-from-content-stream %)))
          (get-objects cs)))

(defn- get-pages-from-document [^PDDocument doc]
  (iterator-seq (.iterator
                 (.getPages doc))))

(defn get-images-from-pdf [pdf]
  (with-open [doc (pdfboxing.common/obtain-document pdf)]
    (mapcat get-images-from-content-stream
            (get-pages-from-document doc))))
