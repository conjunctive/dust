(ns dust.spec
  "Exhaustiveness checking for spec"
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [net.cgrand.xforms :as x]))

(s/def ::sum
  (s/cat :or #{'or}
         :cases (s/+ (s/cat :name keyword?
                            :spec any?))))

(defn details
  "Describe a named or literal spec"
  [spec]
  (-> spec
      (s/get-spec)
      (or spec)
      (s/describe)))

(defn confirm
  "Ensure a sum spec is provided"
  [spec]
  (let [data (s/conform ::sum spec)]
    (when-not (s/invalid? data)
      data)))

(defn cases
  "Derive all possible cases from the sum spec"
  [spec]
  (some->> spec
           (details)
           (confirm)
           :cases))

(defn pair?
  "Predicate for two-element key-value vector"
  [x]
  (and (vector? x)
       (= 2 (count x))
       (keyword? (first x))))

(defn patterns
  "Parse match argument sequence"
  [args]
  (let [xf (comp (take-nth 2)
                 (filter pair?)
                 (map first))]
    (x/into #{} xf args)))

(defn name->spec
  "Produce map of case names onto spec implementations"
  [cases]
  (let [xf (x/by-key :name :spec identity)]
    (x/into {} xf cases)))

(defn missing
  "Find missing specs"
  [spec args]
  (when-let [cases (cases spec)]
    (-> (name->spec cases)
        (x/without (patterns args))
        (not-empty))))

(defn explain
  "Print missing specs as formatted string"
  [missing]
  (->> missing
       (or (:missing missing))
       (reduce (fn [s m] (str s "\n\t" (key m) " of " (val m)))
               "Pattern match is non-exhaustive\nMissing specs:")
       (println)))

(defn exception
  "Throw an exception, carrying missing specs"
  [missing]
  (-> "Pattern match is non-exhaustive"
      (ex-info {:missing missing})
      (throw)))

(defn detect
  "Non-exhaustive pattern detection"
  [spec args]
  (some->> args
           (missing spec)
           (exception)))
