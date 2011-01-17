(ns clojure-dom.test.core
  (:use [clojure-dom.core] :reload-all)
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

(deftest test-dom-root
  (testing "DOM creation and root node."
   (let [n1 (element-node :test nil)
	 n2 (comment-node "comment")
	 n3 (text-node "text")]

     (testing "root node association" 
       (is (= n1 (:root (create-dom n1))))
       (is (belongs? (create-dom n1) n1)))
     
     (testing "illegal root nodes"
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
	 n2 (element-node :node)
	 n3 (element-node :node)
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
       (let [root (element-node :root)
	     dom (-> (create-dom root) (add-child root n1) (add-child root n2)(add-child root n3))]
	 (is (= root (parent dom n1)))
	 (is (= root (parent dom n2)))
	 (is (= root (parent dom n3)))
	 (is (= n1 (first-child dom root)))
	 (is (nil? (previous-sibling dom n1)))
	 (is (= n2 (next-sibling dom n1)))
	 (is (= n1 (previous-sibling dom n2)))
	 (is (= n3 (next-sibling dom n2)))
	 (is (= n3 (last-child dom root)))
	 (is (nil? (next-sibling dom n3)))
	 (is (= n2 (previous-sibling dom n3)))
	 (is (= 3 (count (child-nodes dom root))))))

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

 ; end test-mutate-node

(deftest test-export-dom
  (testing "Exporting DOM sub-structure"
    (let [n0 (element-node :root)
	  c1 (comment-node "comment 1")
	  n2 (element-node :node3)
	  c3 (comment-node "comment 2")
	  t4 (text-node "text 1")
	  n5 (element-node :node5)

	  t21 (text-node "text 2")
	  n22 (element-node :node22)
	  n23 (element-node :node23)

	  c221 (comment-node "comment 3")
	  n222 (element-node :node223)
	  t223 (text-node "text 3")

	  t51 (text-node "blah blah")
	  n52 (element-node :node52)
	  c53 (comment-node "comment bkah blah")

	  dom (-> (create-dom n0)
		  (add-child n0 c1) (add-child n0 n2) (add-child n0 c3)  (add-child n0 t4)  (add-child n0 n5)
		  (add-child n2 t21) (add-child n2 n22) (add-child n2 n23)
		  (add-child n22 c221) (add-child n22 n222) (add-child n22 t223)
		  (add-child n5 t51) (add-child n5 n52) (add-child  n5 c53))
	  ]
      
      (testing "illegal root nodes for export"
	(is (thrown? Exception (export-dom dom (element-node :node))))
	; doesn't belong to DOM
	)

      (testing "Header and footer of exported DOM"
	(is (= (comment-text c1) (:header (export-dom dom n2))))
	(is (= (comment-text c3) (:footer (export-dom dom n2))))
	(is (nil? (:header (export-dom dom n22))))
	(is (nil? (:footer (export-dom dom n22))))
	(is (= (comment-text c221) (:header (export-dom dom n222))))
	(is (nil? (:footer (export-dom dom n222))))
	(is (nil? (:header (export-dom dom n52))))
	(is (= (comment-text c53) (:footer (export-dom dom n52))))
	)
      (testing "Structure of exported DOM"
	(let [domn2 (export-dom dom n2)]
	  (testing "Node maps contain correct nodes"
	    (is (= (:nodes-set domn2) #{n2 t21 n22 n23 c221 n222 t223}))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (keys (:parent-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (vals (:parent-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (keys (:first-child-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (vals (:first-child-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (keys (:last-child-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (vals (:last-child-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (keys (:previous-sibling-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (vals (:previous-sibling-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (keys (:next-sibling-map domn2))))
	    (is (not-any? #{c1 c3 t4 n5 t51 n52 c53} (vals (:next-sibling-map domn2))))
	    )
	  (testing "Root node"
	   (is (= n2 (:root domn2)))
	   (is (nil? (previous-sibling domn2 n2)))
	   (is (nil? (next-sibling domn2 n2)))
	   (is (nil? (parent domn2 n2))))
	  (testing "Parents"
	    (is (= n2 (parent domn2 t21)))
	    (is (= n2 (parent domn2 n22)))
	    (is (= n2 (parent domn2 n23)))
	    (is (= n22 (parent domn2 c221)))
	    (is (= n22 (parent domn2 n222)))
	    (is (= n22 (parent domn2 t223))))
	  (testing "First and last child"
	    (is (= t21 (first-child domn2 n2)))
	    (is (= n23 (last-child domn2 n2)))
	    (is (nil? (first-child domn2 n23)))
	    (is (nil? (last-child domn2 n23)))
	    (is (= c221 (first-child domn2 n22)))
	    (is (= t223 (last-child domn2 n22))))
	  (testing "Previous and next sibling"
	    (is (= t21 (previous-sibling domn2 n22)))
	    (is (= n23 (next-sibling domn2 n22)))
	    (is (nil? (previous-sibling domn2 t21)))
	    (is (nil? (next-sibling domn2 n23)))
	    (is (= n22 (previous-sibling domn2 n23)))
	    (is (= n22 (next-sibling domn2 t21)))

	    (is (= c221 (previous-sibling domn2 n222)))
	    (is (= t223 (next-sibling domn2 n222)))
	    (is (nil? (previous-sibling domn2 c221)))
	    (is (nil? (next-sibling domn2 t223)))
	    (is (= n222 (previous-sibling domn2 t223)))
	    (is (= n222 (next-sibling domn2 c221))))))
      ))) ; end test-export-dom

(deftest test-mutate-node
  (let [n1 (element-node :root)
	  c11 (comment-node "c11")
	  n12 (element-node :n12 :a 1)
	  t13 (text-node "t13")
	  n121 (element-node :n121)
	  n122 (element-node :n122)
	  dom (-> (create-dom n1)
		  (add-child n1 c11)(add-child n1 n12)(add-child n1 t13)
		  (add-child n12 n121)(add-child n12 n122))]
    (testing "Illegal nodes"
      (is (thrown? Exception (mutate-node dom (element-node :test) (element-node :blah))))
      (is false) ; TODO
      ))


  ) ; end test-mutate-node