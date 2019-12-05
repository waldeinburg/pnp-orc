(ns pnp-orc.tools
  "Functions for preparing recipes"
  (:require [mikera.image.core  :as img]
            [pnp-orc.file      :as file]
            [pnp-orc.pdf       :as pdf]))

(defn dump-images-from-pdf [pdf folder]
  "Dump all images from a PDF."
  (let [images (pdf/get-images-from-pdf pdf)]
    (file/save-images
      images (str (clojure.java.io/file folder "img-%03d.png")))))

(defn render-page-to-file
  "Render a page to an image file.
   Useful for determining coordinates based on cut lines for PDF's
   containing several images per page."
  ([pdf page-idx image-idx file]
   (let [dpi (pdf/get-bitmap-dpi pdf page-idx image-idx)
         image (pdf/render-page-to-image pdf dpi page-idx)]
     (img/save image file))))

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
