(ns dust.core
  (:require [dust.match]
            [dust.spec]))

(defmacro spec
  {:style/indent 1}
  [data spec & body]
  `(clojure.core.match/match (clojure.spec.alpha/conform ~spec ~data)
     :clojure.spec.alpha/invalid :clojure.spec.alpha/invalid
     ~@body))

(defmacro sum
  {:style/indent 1}
  [data spec & body]
  (dust.spec/detect spec body)
  `(clojure.core.match/match (clojure.spec.alpha/conform ~spec ~data)
     :clojure.spec.alpha/invalid :clojure.spec.alpha/invalid
     ~@body))
