(ns task01.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]))

(defn read-file-lines [name]
  (with-open [rdr (clojure.java.io/reader name)]
    (reduce conj [] (line-seq rdr))))

(defn solve01 [dim numbers]
  (->> (apply combo/cartesian-product (repeat dim numbers))
       (filter #(= (apply + %) 2020))
       (first)
       (apply *)))

(defn -main [& args]
  (->> (read-file-lines "01.input")
       (map read-string)
       (solve01 3)                      ; 2 to solve first task, 3 to solve second task
       (str)
       (print)))
