(defproject pnp-orc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [net.mikera/imagez "0.12.0"]
                 ; this uses PDFBox 2.0.8 and should be updated when possible
                 [pdfboxing "0.1.14.1-SNAPSHOT"]] 
  :main ^:skip-aot pnp-orc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
