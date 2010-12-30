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
    (is (valid? (text-node nil)))
    (is (= "blah2" (:text (text-node "blah2"))))
    (is (= "3" (:text (text-node 3))))
    (is (= "" (:text (text-node nil))))))

(deftest test-comment-node
  (testing "Creating comment nodes"
    (is (valid? (comment-node "blah")))
    (is (valid? (comment-node nil)))
    (is (= "blah2" (:comment (comment-node "blah2"))))
    (is (= "4" (:comment (comment-node 4))))
    (is (= "" (:comment (comment-node nil))))))

(deftest test-element-node
  (testing "Creating element nodes"
    (testing "valid nodes"
      (is (valid? (element-node :test)))
      (is (valid? (element-node "test" )))
      (is (valid? (element-node :test nil)))
      (is (valid? (element-node "test" nil)))
      (is (valid? (element-node :test {:a "blah"}))))
    (testing "element parameter"
      (is (= :blah (:element (element-node :blah))))
      (is (= :blah2 (:element (element-node "blah2"))))
      (is (= :blah (:element (element-node :blah nil))))
      (is (= :test (:element (element-node "test" nil))))
      (is (= :test (:element (element-node "test" :a "blah"))))
      (is (= :test (:element (element-node :test :a "blah"))))
      (is (= :test2 (:element (element-node "test2" {:a "blah"}))))
      (is (= :test3 (:element (element-node :test3 {:a "blah"}))))
      (is (= :test (:element (element-node "test" :a "blah" :b "b" :c "test2"))))
      (is (= :test (:element (element-node :test :a "blah" :b "b" :c "test2")))))

    (testing "element attributes"
      (is (= "blah" (attribute (element-node "test2" {:a "blah"}) :a)))
      (is (nil? (:attributes (element-node :test nil))))
      (is (nil? (:attributes (element-node :test {}))))
      (is (nil? (attribute (element-node "test2" {:a "blah"}) :ba)))
      (is (= 3 (count (:attributes (element-node :test :a "blah" :b "b" :c "test2")))))
      (is (= "blah" (attribute (element-node :test :a "blah" :b "b" :c "test2") :a)))
      (is (= "b" (attribute (element-node :test :a "blah" :b "b" :c "test2") :b)))
      (is (= "test2" (attribute (element-node :test :a "blah" :b "b" :c "test2") :c))))
    
    (testing "invalid nodes"
      (is (thrown? Exception (element-node nil)))
      (is (thrown? Exception (element-node 10)))
      (is (thrown? Exception (element-node :elem :a 1 :b)))
      (is (thrown? Exception (element-node :blah [:a 1]))))
    )) ; end test-element-node

(deftest test-dom-root
  (testing "DOM creation and root node."
   (let [n1 (clojure-dom.core.Node. nil nil :test nil)
	 n2 (comment-node "comment")
	 n3 (text-node "text")]

     (testing "root node association" 
       (is (= n1 (:root (create-dom n1))))
       (is (belongs? (create-dom n1) n1)))
     
     (testing "illegal root nodes"
       (is (thrown? Exception (create-dom n2)))
       (is (thrown? Exception (create-dom n3)))
       (is (thrown? Exception (create-dom nil))))
     )))
					; end test-dom-root
(deftest test-dom-header
  (let [dom (create-dom)]
    (testing "DOM without header comment"
     (is (nil? (:header dom)))
     (is (nil? (previous-sibling dom (:root dom))))
     (is (nil? (next-sibling dom (:root dom))))
     ))
  (let [dom (add-header (create-dom) nil)]
    (testing "DOM with a nil header comment"
     (is (nil? (:header dom)))
     (is (nil? (previous-sibling dom (:root dom))))
     (is (nil? (next-sibling dom (:root dom))))
     ))
  (let [dom (add-header (create-dom) "header test")]
    (testing "DOM with a header comment"
     (is (= "header test" (:header dom)))
     (is (nil? (previous-sibling dom (:root dom))))
     (is (nil? (next-sibling dom (:root dom))))
     (is (nil? (:footer dom)))
     (is (= "test2" (:header (add-header dom "test2"))))
     (is (nil? (:header (add-header dom nil))))
     ))
  ) ; end test-dom-header

(deftest test-dom-footer
  (let [dom (create-dom)]
    (testing "DOM without footer comment"
     (is (nil? (:footer dom)))
     (is (nil? (previous-sibling dom (:root dom))))
     (is (nil? (next-sibling dom (:root dom))))
     ))
  (let [dom (add-footer (create-dom) nil)]
    (testing "DOM with a nil footer comment"
     (is (nil? (:footer dom)))
     (is (nil? (previous-sibling dom (:root dom))))
     (is (nil? (next-sibling dom (:root dom))))
     ))
  (let [dom (add-footer (create-dom) "footer test")]
    (testing "DOM with a footer comment"
     (is (= "footer test" (:footer dom)))
     (is (nil? (next-sibling dom (:root dom))))
     (is (nil? (previous-sibling dom (:root dom))))
     (is (nil? (:header dom)))
     (is (= "test2" (:footer (add-footer dom "test2"))))
     (is (nil? (:header (add-footer dom nil))))
     ))
  ) ; end test-dom-footer

(deftest test-dom-comments
  (testing "DOM with both header and footer comments"
    (is (= "header" (:header (add-comments (create-dom) "header" "footer"))))
    (is (= "footer" (:footer (add-comments (create-dom) "header" "footer"))))
    (is (= "header" (:header (add-comments (create-dom) "header" nil))))
    (is (= "footer" (:footer (add-comments (create-dom) nil "footer"))))
    (is (nil? (:footer (add-comments (create-dom) "header" nil))))
    (is (nil? (:header (add-comments (create-dom) nil "footer"))))
    ))

(deftest test-add-child
  (testing "add-child function"
   (let [n1 (element-node :root)
	 n2 (element-node :n2)
	 n3 (element-node :n3)
	 n4 (comment-node "comment")
	 n5 (text-node "text")]
     
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

     (testing "moving a node by adding it to another parent"
       (let [dom (-> (create-dom n1) (add-child n1 n2)
		     (add-child n2 n3) (add-child n3 n4) (add-child n3 n5))]
	 (testing "moving a single node"
	   (is (= n5 (-> dom (add-child n2 n5) (last-child n2))))
	   (is (= n2 (-> dom (add-child n2 n5) (parent n5))))
	   (is (= n3 (-> dom (add-child n2 n5) (previous-sibling n5))))
	   (is (= n5 (-> dom (add-child n2 n5) (next-sibling n3))))
	   (is (= n4 (-> dom (add-child n2 n5) (first-child n3))))
	   (is (= n4 (-> dom (add-child n2 n5) (last-child n3))))
	   (is (nil? (-> dom (add-child n2 n5) (previous-sibling n4))))
	   (is (nil? (-> dom (add-child n2 n5) (next-sibling n4))))
	   )	 
	 (testing "moving a node with children"
	   (is (= n3 (-> dom (add-child n1 n3) (last-child n1))))
	   (is (= n1 (-> dom (add-child n1 n3) (parent n3))))
	   (is (= n2 (-> dom (add-child n1 n3) (previous-sibling n3))))
	   (is (= n3 (-> dom (add-child n1 n3) (next-sibling n2))))
	   (is (= n3 (-> dom (add-child n1 n3) (parent n4))))
	   (is (= n3 (-> dom (add-child n1 n3) (parent n5))))
	   (is (nil? (-> dom (add-child n1 n3) (first-child n2))))
	   (is (nil? (-> dom (add-child n1 n3) (last-child n2))))
	   )
	 (testing "moving a node without changing the parent"
	   (is (= n5 (-> dom (add-child n3 n5) (last-child n3))))
	   (is (= n4 (-> dom (add-child n3 n5) (first-child n3))))
	   (is (= n3 (-> dom (add-child n3 n5) (parent n5))))
	   (is (= n4 (-> dom (add-child n3 n5) (previous-sibling n5))))
	   (is (= n5 (-> dom (add-child n3 n5) (next-sibling n4))))
	   (is (nil? (-> dom (add-child n3 n5) (next-sibling n5))))
	   
	   (is (= n5 (-> dom (add-child n3 n4) (first-child n3))))
	   (is (= n4 (-> dom (add-child n3 n4) (last-child n3))))
	   (is (nil? (-> dom (add-child n3 n4) (previous-sibling n5))))
	   (is (= n4 (-> dom (add-child n3 n4) (next-sibling n5))))	   
	   (is (nil? (-> dom (add-child n3 n4) (next-sibling n4))))
	   (is (= n5 (-> dom (add-child n3 n4) (previous-sibling n4))))
	   )
	 (testing "moving illegal nodes"
	   (is (thrown? Exception (add-child dom n3 n1)))
	   (is (thrown? Exception (add-child dom n3 n2)))
	   (is (thrown? Exception (add-child dom n3 n3)))))))
     ))
					; end test-add-child

