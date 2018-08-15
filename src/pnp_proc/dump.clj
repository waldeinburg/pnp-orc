(ns pnp-proc.dump
  "Functions for dumping data for preparing recipies"
  (:require [mikera.image.core :as img]
            [pnp-proc.worker   :as worker]
            [pnp-proc.pdf      :as pdf])
  (:import pnp_proc.BitmapDpiFinder
           org.apache.pdfbox.rendering.PDFRenderer))

(defn dump-images-from-pdf [pdf folder]
  "Dump all images from a PDF.
   For preparing recipies."
  (let [images (pdf/get-images-from-pdf pdf)]
    (worker/save-images
     images (str (clojure.java.io/file folder "img-%03d.png")))))

(defn render-page-to-image
  "Render a page to an image file.
   Useful for determining coordinates based on cut lines for PDF's
   containing several images per page."
  ([pdf page-idx file]
   (pdf/with-open-doc [doc pdf]
     (let [renderer (PDFRenderer. doc)
           page (.getPage doc page-idx)
           dpi (.getBitmapDpi (BitmapDpiFinder.) page)
           image (.renderImageWithDPI renderer page-idx dpi)]
       (img/save image file)))))
