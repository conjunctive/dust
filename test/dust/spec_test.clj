(ns dust.spec-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [dust.spec :as ds]))

;; Sum type spec
(s/def ::name-or-id
  (s/or :name string?
        :id int?))

(deftest spec
  (testing "Case retrieval"
    (testing "With literal specs"
      (is (nil? (ds/cases (s/and int? string?))))
      (is (= [:a :b]
             (->> (s/or :a int? :b string?)
                  (ds/cases)
                  (mapv :name)))))

    (testing "With named specs"
      (s/def ::invalid (s/and int? string?))
      (is (nil? (ds/cases ::invalid)))
      (s/def ::valid (s/or :a int? :b string?))
      (is (= [:a :b]
             (mapv :name (ds/cases ::valid))))))

  (testing "Parsing of match argument sequence"
    (is (= #{:name :id}
           (ds/patterns
            (seq [[:name 's] 's
                  [:id 'i] 'i
                  :else nil]))))
    (testing "Drops non-keywords from vector pair patterns"
      (is (= #{:id}
             (ds/patterns
              (seq [['name 's] 's
                    [:id 'i] 'i
                    :else nil]))))))

  (testing "Detection of missing patterns"
    (is (nil? (ds/missing ::name-or-id
                          (seq [[:name 's] 's
                                [:id 'i] 'i]))))

    (is (= (list :id)
           (->> (seq [[:name 's] 's])
                (ds/missing ::name-or-id)
                (keys))))

    (let [spec (s/or :a string? :b float?)]
      (= (ds/name->spec (ds/cases spec))
         (ds/missing spec (seq [[:x 'x] 'x [:y 'y] 'y])))))

  (testing "Exceptional on missing patterns"
    (let [spec ::name-or-id
          data (cons [:x :y] (seq {:name 1}))]
      (try (ds/detect spec data)
           (catch Exception e
             (testing "With missing specs carried"
               (is (= (:missing (ex-data e))
                      (ds/missing spec data)))))))))
