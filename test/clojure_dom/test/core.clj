(ns clojure-dom.test.core
  (:use [clojure-dom.core] :reload)
  (:use [clojure.test]))

(deftest test-node-valid?
  (testing "Node validation"  
    (testing "valid nodes"
     (is (valid? (clojure-dom.core.Node. "blah" nil nil nil)))
     (is (valid? (clojure-dom.core.Node. nil "balh" nil nil)))
     (is (valid? (clojure-dom.core.Node. nil nil :test nil)))
     (is (valid? (clojure-dom.core.Node. nil nil :node {:name "blah"}))))

    (testing "invalid nodes"
      (is (false? (valid? (clojure-dom.core.Node. nil nil nil nil))))
      (is (false? (valid? (clojure-dom.core.Node. "test" nil :test nil))))
      (is (false? (valid? (clojure-dom.core.Node. nil "blah" :test nil))))
      (is (false? (valid? (clojure-dom.core.Node. "balh" "test" nil nil))))
      (is (false? (valid? (clojure-dom.core.Node. nil nil "element" nil))))
      (is (false? (valid? (clojure-dom.core.Node. "test" nil nil {:attr "blah"}))))
      (is (false? (valid? (clojure-dom.core.Node. nil "blah" nil {:attr "blah"}))))
      (is (false? (valid? (clojure-dom.core.Node. nil nil nil {:attr "blah"}))))
      (is (false? (valid? (clojure-dom.core.Node. nil nil :test [:attr "bla"]))))
      (is (false? (valid? (clojure-dom.core.Node. nil nil :element "attributes")))))
    ))
					; end test-node-valid?

(deftest test-text-node
  (testing "Creating text nodes"
    (is (valid? (text-node "blah")))
    (is (= "blah2" (:text (text-node "blah2"))))
    (is (nil? (text-node 3)))))

(deftest test-comment-node
  (testing "Creating comment nodes"
    (is (valid? (comment-node "blah")))
    (is (= "blah2" (:comment (comment-node "blah2"))))
    (is (nil? (comment-node 3)))))

(deftest test-element-node
  (testing "Creating element nodes"
    (is (valid? (element-node :test nil)))
    (is (valid? (element-node :test {:a "blah"})))
    ))

(deftest test-dom-root
  (testing "DOM creation and root node."
   (let [n1 (clojure-dom.core.Node. nil nil :test nil)
	 n2 (clojure-dom.core.Node. "comment" nil nil nil)
	 n3 (clojure-dom.core.Node. nil "text" nil nil)]

     (testing "root node association" 
       (is (= n1 (:root (create-dom n1))))
       (is (belongs? (create-dom n1) n1)))
     
     (testing "illegal root nodes"
       (is (thrown? Exception (create-dom n2)))
       (is (thrown? Exception (create-dom n3)))
       (is (thrown? Exception (create-dom nil))))
     )))
					; end test-dom-root

(deftest test-add-child
  (testing "add-child function"
   (let [n1 (clojure-dom.core.Node. nil nil :root nil)
	 n2 (clojure-dom.core.Node. nil nil :n2 nil)
	 n3 (clojure-dom.core.Node. nil nil :n3 nil)
	 n4 (clojure-dom.core.Node. "comment" nil nil nil)
	 n5 (clojure-dom.core.Node. nil "text" nil nil)]
     
     (testing "single child"
       (let [dom (add-child (create-dom n1) n1 n4)]
	 (is (= n1 (:root dom)))
	 (is (= n1 (parent dom n4)))
	 (is (= n4 (first-child dom n1)))
	 (is (= n4 (last-child dom n1)))))
     
     (testing "in depth"
       (let [dom (add-child (add-child (create-dom n1) n1 n2) n2 n3)]
	 (is (= n1 (parent dom n2)))
	 (is (= n2 (parent dom n3)))
	 (is (= n2 (first-child dom n1)))
	 (is (= n3 (first-child dom n2)))))

     (testing "illegal parent"
       (is (thrown? Exception (add-child (create-dom n1) n2 n3)))
       (is (thrown? Exception (add-child (create-dom n1) nil n3))))

     (testing "illegal child"
       (is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (add-child n1 n1))))
       (is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (add-child n1 n2))))
       (is (thrown? Exception (-> (create-dom n1) (add-child n1 nil))))
       (is (thrown? Exception (-> (create-dom n1) (add-child n1 "blah")))))

     (testing "multiple child nodes for a root"
       (let [root n2
	     dom (-> (create-dom root) (add-child root n1) (add-child root n3))]
	 (is (= root (parent dom n1)))
	 (is (= root (parent dom n3)))
	 (is (= n1 (first-child dom root)))
	 (is (= n3 (last-child dom root)))
	 (is (= 2 (count (child-nodes dom root))))))

     (testing "multiple child nodes"
       (let [dom (-> (create-dom n1) (add-child n1 n2)
		     (add-child n2 n3) (add-child n2 n4) (add-child n2 n5))]
	 (is (= n2 (parent dom n3)))
	 (is (= n2 (parent dom n4)))
	 (is (= n2 (parent dom n5)))
	 (is (= n3 (first-child dom n2)))
	 (is (= n5 (last-child dom n2)))
	 (is (= 3 (count (child-nodes dom n2))))
	 (is (= n3 (first (child-nodes dom n2))))
	 (is (= n4 (second (child-nodes dom n2))))
	 (is (= n5 (last (child-nodes dom n2))))))     
     )))
					; end test-add-child

(deftest test-insert-after
  (testing "Testing insert-after function"
    (let [n1 (clojure-dom.core.Node. nil nil :root nil)
	  n2 (clojure-dom.core.Node. nil nil :n2 nil)
	  n3 (clojure-dom.core.Node. nil nil :n3 nil)
	  n4 (clojure-dom.core.Node. "comment" nil nil nil)
	  n5 (clojure-dom.core.Node. nil "text" nil nil)]

      (testing "illegal node"
	(is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (insert-after n1 n3))))
	(is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (insert-after n3 n4))))
	(is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (insert-after nil n3))))
	(is (thrown? Exception (-> (create-dom n1)
				   (add-child n1 n2) (add-child n2 n3) (add-child n3 n4)
				   (insert-after n4 n2))))
	(is (thrown? Exception (-> (create-dom n1)
				   (add-child n1 n2) (add-child n2 n3) (add-child n3 n4)
				   (insert-after n4 n1))))
	(is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (insert-after n2 nil))))
	(is (thrown? Exception (-> (create-dom n1) (add-child n1 n2) (insert-after n2 {})))))
      )))
 ; end test-insert-after 