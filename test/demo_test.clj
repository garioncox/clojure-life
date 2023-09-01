(ns demo-test
  (:require [clojure.test :refer :all]
            [demo :refer :all]))

(deftest test-add
  (is (= 5 (add 2 3))))

;; to run:
;; clj -X <file>/<function>

;; to test:
;; clj -X:test