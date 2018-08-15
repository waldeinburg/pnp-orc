;;; The reason this one-time-use functionality is not implemented with
;;; a proxy is that we need to call the super class in an overrided method.
(ns pnp-proc.BitmapDpiFinder
  "Class to find the correct scale for dumping a page as image
   Based on org.apache.pdfbox.examples.util.PrintImageLocations"
  (:import org.apache.pdfbox.pdmodel.PDPage
           org.apache.pdfbox.contentstream.operator.DrawObject
           [org.apache.pdfbox.contentstream.operator.state
            Concatenate Restore Save SetGraphicsStateParameters SetMatrix]
           org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject)
  (:gen-class
   :extends org.apache.pdfbox.contentstream.PDFStreamEngine
   :state dpi
   :init init
   :post-init post-init
   :exposes-methods {processOperator superProcessOperator}
   :methods [[getBitmapDpi [] java.lang.Double]]))

(import 'pnp_proc.BitmapDpiFinder)

(defn -init []
  [[] (atom false)])

(defn -post-init [^BitmapDpiFinder this]
  ;; Yes, they are all necessary.
  (.addOperator this (Concatenate.))
  (.addOperator this (DrawObject.))
  (.addOperator this (SetGraphicsStateParameters.))
  (.addOperator this (Save.))
  (.addOperator this (Restore.))
  (.addOperator this (SetMatrix.)))

(defn -getBitmapDpi [^BitmapDpiFinder this
                     ^PDPage page]
  ;; Reset state
  (reset! (.dpi this) false)
  (.processPage this page)
  @(.dpi this))

(defn- -processOperator [^BitmapDpiFinder this
                         operator operands]
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
          (swap! (.dpi this) #(or % %2) new-dpi))))
    (.superProcessOperator this operator operands)))
