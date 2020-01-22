(ns dust.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.match :as m]
            [clojure.spec.alpha :as s]
            [clojure.set :as set]
            [dust.core :as dust]))

;; Recursive sum type specs
(s/def ::sum (constantly false))
(s/def ::a int?)
(s/def ::b float?)
(s/def ::c
  (s/coll-of ::sum))
(s/def ::d
  (s/keys :req-un [::a ::b ::c]))
(s/def ::sum
  (s/or :a ::a
        :b ::b
        :c ::c
        :d ::d))

(deftest core
  (testing "Pattern matching on spec output"

    (testing "With sum type"
      (letfn [(name-or-id [x]
                (dust/sum x
                  (s/or :name string? :id int?)
                  [:name s] s
                  [:id n] n))]
        (is (= "abc" (name-or-id "abc")))
        (is (= 123 (name-or-id 123)))))

    (testing "With recursive sum type"
      (is (= 23
             (dust/spec 23
               ::sum
               [:a a] a
               :else nil)))

      (is (= 23.5
             (dust/sum 23.5
               ::sum
               [:a a] a
               [:b b] b
               [:c c] c
               [:d d] d)))

      (is (= 2.5
             (dust/spec [23 23.5 {:a 1 :b 1.5 :c [2 2.5]}]
               ::sum
               [:c [[:a 23]
                    [:b 23.5]
                    [:d {:a 1
                         :b 1.5
                         :c [[:a 2]
                             [:b c->d->c->b]]}]]]
               c->d->c->b
               :else nil)))
      (is (= 7.0
             (dust/spec {:a 1 :b 1.5 :c [2 2.5]}
               ::sum
               [:d {:a d->a
                    :b d->b
                    :c [[:a d->c->a]
                        [:b d->c->b]]}]
               (+ d->a
                  d->b
                  d->c->a
                  d->c->b)
               :else nil))))))
