(ns dust.match
  "Map-entry integration for core.match"
  (:require [clojure.core.match :as m]
            [clojure.core.match.protocols :as mp]))

(defn handle-map-entry [this k not-found]
  (case (key this)
    k (val this)
    not-found))

(extend-type clojure.lang.MapEntry
  mp/IMatchLookup
  (val-at [this k not-found]
    (handle-map-entry this k not-found))
  mp/ISyntaxTag
  (syntax-tag [_]
    ::m/map-entry))

(defrecord MapEntryPattern [pat])

(defn map-entry-pattern [map-entry]
  (-> (MapEntryPattern. map-entry)
      (assoc ::m/tag ::m/map-entry)))

(defmethod m/emit-pattern ::m/map-entry [pat]
  (map-entry-pattern pat))

(defmethod m/groupable? [::m/map-entry ::m/map-entry] [a b]
  (let [^clojure.lang.MapEntry ma (:map-entry a)
        ^clojure.lang.MapEntry mb (:map-entry b)]
    (= ma mb)))
