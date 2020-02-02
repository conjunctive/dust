(ns dust.core
  (:require [clojure.core.match :as m]
            [clojure.spec.alpha :as s]
            [dust.spec]))

(defmacro spec
  {:style/indent 1}
  [data spec & body]
  `(m/match (s/conform ~spec ~data)
     ::s/invalid ::s/invalid
     ~@body))

(defmacro sum
  {:style/indent 1}
  [data spec & body]
  (dust.spec/detect spec body)
  `(m/match (s/conform ~spec ~data)
     ::s/invalid ::s/invalid
     ~@body))

(defmacro rune
  {:style/indent 1}
  [data spec & body]
  `(do (dust.spec/detect ~spec (seq '~body))
       (m/match (s/conform ~spec ~data)
         ::s/invalid ::s/invalid
         ~@body)))
