(ns clojure-dom.test.node
  (:use [clojure-dom.node] :reload-all)
  (:use [clojure.test]))

(deftest test-text-node
  (testing "Creating text nodes"
    (is (xml-valid? (text-node "blah")))
    (is (xml-valid? (text-node 3)))
    (is (xml-valid? (text-node nil)))
    (is (and (text? (text-node "blah"))
	     (not (comment? (text-node "blah")))
	     (not (element? (text-node "blah")))))
    (is (and (text? (text-node nil))
	     (not (comment? (text-node nil)))
	     (not (element? (text-node nil)))))
    (is (and (text? (text-node 3))
	     (not (comment? (text-node 3)))
	     (not (element? (text-node 3)))))
    (is (= "blah2" (text (text-node "blah2"))))
    (is (= "3" (text (text-node 3))))
    (is (= "" (text (text-node nil))))))

(deftest test-comment-node
  (testing "Creating text nodes"
    (is (xml-valid? (comment-node "blah")))
    (is (xml-valid? (comment-node 3)))
    (is (xml-valid? (comment-node nil)))
    (is (and (comment? (comment-node "blah"))
	     (not (text? (comment-node "blah")))
	     (not (element? (comment-node "blah")))))
    (is (and (comment? (comment-node nil))
	     (not (text? (comment-node nil)))
	     (not (element? (comment-node nil)))))
    (is (and (comment? (comment-node 3))
	     (not (text? (comment-node 3)))
	     (not (element? (comment-node 3)))))
    (is (= "blah2" (comment-text (comment-node "blah2"))))
    (is (= "3" (comment-text (comment-node 3))))
    (is (= "" (comment-text (comment-node nil))))))

(deftest test-element-node
  (testing "Creating element nodes"
    (testing "valid nodes"
      (is (xml-valid? (element-node :test)))
      (is (xml-valid? (element-node "test" )))
      (is (xml-valid? (element-node :test nil)))
      (is (xml-valid? (element-node "test" nil)))
      (is (xml-valid? (element-node :test {:a "blah"}))))
    (testing "element parameter"
      (is (= :blah (element (element-node :blah))))
      (is (= :blah2 (element (element-node "blah2"))))
      (is (= :blah (element (element-node :blah nil))))
      (is (= :test (element (element-node "test" nil))))
      (is (= :test (element (element-node "test" :a "blah"))))
      (is (= :test (element (element-node :test :a "blah"))))
      (is (= :test2 (element (element-node "test2" {:a "blah"}))))
      (is (= :test3 (element (element-node :test3 {:a "blah"}))))
      (is (= :test (element (element-node "test" :a "blah" :b "b" :c "test2"))))
      (is (= :test (element (element-node :test :a "blah" :b "b" :c "test2")))))

    (testing "element attributes"
      (is (= "blah" (attribute (element-node "test2" {:a "blah"}) :a)))
      (is (nil? (attributes (element-node :test nil))))
      (is (nil? (attributes (element-node :test {}))))
      (is (nil? (attribute (element-node "test2" {:a "blah"}) :ba)))
      (is (= 3 (count (attributes (element-node :test :a "blah" :b "b" :c "test2")))))
      (is (= "blah" (attribute (element-node :test :a "blah" :b "b" :c "test2") :a)))
      (is (= "b" (attribute (element-node :test :a "blah" :b "b" :c "test2") :b)))
      (is (= "test2" (attribute (element-node :test :a "blah" :b "b" :c "test2") :c))))
    
    (testing "invalid nodes"
      (is (thrown? Exception (element-node nil)))
      (is (thrown? Exception (element-node 10)))
      (is (thrown? Exception (element-node :elem :a 1 :b)))
      (is (thrown? Exception (element-node :blah [:a 1]))))
    )) ; end test-element-node
