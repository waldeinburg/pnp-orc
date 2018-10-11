(ns pnp-orc.core
  (:gen-class))

(defn -main
  "Take the following parameters:
  FOR SOURCE
  folder
  offset x of second card
  offset y of second card
  card width
  card height
  columns
  rows
  FOR TARGET
  number of cards in one page
  horizontal and vertical margin
  horzontal spacing
  vertical spacing to fold line"
  [& args]
  (println "Hello, World!"))
