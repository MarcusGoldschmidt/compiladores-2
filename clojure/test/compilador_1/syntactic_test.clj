(ns compilador-1.syntactic-test
  (:require [clojure.test :refer :all]
            [compilador-1.syntactic :refer :all]))

(defn- success-token [token]
  (is (false? (:error token))))

(defn- error-token [token]
  (is (true? (:error token))))

(deftest tipo-var-test
  (testing "Should erro when not real or integer"
           (error-token ((tipo-var)
                          {:tokens [{:match "any", :type :key}]
                           :s-tree {}})))
  (testing "Should erro when not real or integer"
           (is (= 0 1))))
