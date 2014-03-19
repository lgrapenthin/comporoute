(ns comporoute.util
  (:require [clojure.string :as str]))

(defn compose-path
  "Multi-purpose path-builder:

  Split sub-paths into more sub-paths if they contain slashes, replace
  batched slashes with a single slash, concat all sub-paths with
  slashes between them.

  If no sub-paths could be composed, return the empty-string.

  Sub-paths must be strings and may not be nil."
  [& sub-paths]
  (let [path (->> sub-paths
                  (mapcat #(str/split % #"/"))
                  (keep not-empty)
                  (str/join "/"))]
    (if (empty? path)
      ""
      (str "/" path))))
