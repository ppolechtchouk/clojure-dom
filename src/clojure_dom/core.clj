(ns clojure-dom.core
  (:use clojure.test))



(defprotocol Validation
  "Functions that test node and dom internal consistency"
  (valid? [x] "Returns true if x is valid. x can be either a Node or Dom."))

(defrecord Node
    [comment text element attributes]
  Validation
  (valid? [this]
	  (not
	   (cond
	    
	    comment
	    (or text element attributes)
	    
	    text
	    (or comment element attributes)

	    attributes
	    (or (not element) text comment (not (map? attributes)))
	    
	    element
	    (or comment text (not (keyword? element)))

	    :default
	    true ; i.e. not valid
	    ))) ; end valid?
  ) ; end Node

(defprotocol DomAccess
  "Various functions for accessing DOM structure."
  (belongs? [dom n] "Returns true if node is present in the dom.")
  (parent [dom n] "Returns the parent node of n or nil if none")
  (parent-nodes [dom n] "Returns a sequence of parent nodes of n, starting from the immediate parent and all the way up to the root node")
  (next-sibling [dom n] "Retuns the next sibling node or nil if none")
  (previous-sibling [dom n] "Retuns the next sibling node or nil if none")
  (first-child [dom n] "Returns the first child of the node, or nil if none")
  (last-child [dom n] "Returns the last child of the node, or nil if none")
  (child-nodes [dom n] "Returns a sequence of the child nodes.")
  )

(defprotocol DomModification
  "Various functions for creating a new DOM instance with a modified structure."

  (add-child [dom np nc] "Returns a new DOM with the child node (nc) added to the parent node (np). Note that nc will be the last child of np if np already has children. Throws an exception if np is not a valid parent node or nc already belongs to the DOM.")
  (insert-after [dom n1 n2] "Returns a new DOM with n2 as the next sibling of n1. If n2 is already part of the DOM structure, it will be moved to a new location. n1 can not be a root node and must belong to the DOM otherwise an exception is thrown. n2 can not be one of the parent nodes of n1.")
)

(defn verify-parent
  "Returns true if n is a valid parent node - i.e. element node and belongs to the DOM"
  [dom n]
  (and
   (= (class n) Node)
   (:element n)
   (belongs? dom n)))

(defrecord Dom 
    [root parent-map first-child-map last-child-map previous-sibling-map next-sibling-map nodes-set]
  DomAccess
  (belongs? [dom n]
	    (contains? nodes-set n))
  (parent [dom n] (parent-map n))
  (parent-nodes [dom n]
	    (take-while #(not (nil? %)) (iterate parent-map n)))
  (next-sibling [dom n]
		(next-sibling-map n))
  (previous-sibling [dom n]
		    (previous-sibling-map n))
  (first-child [dom n]
	       (first-child-map n))
  (last-child [dom n]
	       (last-child-map n))
  (child-nodes [dom n]
	    (take-while #(not (nil? %)) (iterate next-sibling-map (first-child-map n))))

  DomModification
   (add-child [this np nc]
	      (cond
					; check parent node
	       (not (verify-parent this np))
	       (throw (Exception. (str np " is not a valid parent node")))

					;check child node
	       (belongs? this nc)
	       (throw (Exception. (str "Child node " nc " already belongs to this DOM")))

	       (not (instance? Node nc))
	       (throw (Exception. (str nc " is not a valid child node")))

					; both np and nc are ok
	       :default
	       (let [fc (first-child-map np)
		     lc (last-child-map np)]
		  (cond
		   fc ; already has children
		   (new Dom
			root			     ; root
			(assoc parent-map nc np)     ; parent
			first-child-map ; first child - no change
			(assoc last-child-map np nc) ; last child
			(assoc previous-sibling-map nc lc) ; prev sibling
			(assoc next-sibling-map lc nc)	; next-sibling
			(conj nodes-set nc)	       ; nodes
			)

		   :default ; parent node has no children
		   (new Dom
			root			     ; root
			(assoc parent-map nc np)     ; parent
			(assoc first-child-map np nc) ; first child
			(assoc last-child-map np nc) ; last child
			previous-sibling-map ; prev sibling
			next-sibling-map ; next-sibling
			(conj nodes-set nc)	       ; nodes
			)))
	       )) ; end add-child
   
   (insert-after [this n1 n2]
		 (cond
		  ; validation checks
		  (= root n1)
		  (throw (Exception. (str n1 " is a root node")))
	 
		  (not (belongs? this n1))
		  (throw (Exception. (str n1 " is not part of the DOM structure")))

		  (= n1 n2)
		  (throw (Exception. (str "Attempting to insert " n1 " after itself.")))

		  (contains? (set (parent-nodes this n1)) n2)
		  (throw (Exception. (str n2 " is one of the parent nodes of " n1)))

		  ; everything ok
		  :default
		  nil
		  )	
		 )) ; end insert-after

(defn validate-root
  "Returns true if root node is valid"
  [n]
  (and (instance? Node n)
       (:element n)))

(defn create-dom
  "Creates a DOM ready to be filled with nodes. If you do not supply a root node, a dummy node will be created."
  ([] (create-dom (Node. nil nil :root nil)))
  ([n]
   (if (validate-root n)
    (new Dom
	 n			       ; root node
	 {}			       ; parent-map
	 {}			       ; first-child-map
	 {}                            ; last-child-map  
	 {}			       ; previous-sibling-map
	 {}			       ; next-sibling-map
	 #{n}			       ; nodes that belong to this DOM
	 )
    (throw (Exception. (str "Node " n  " is not a valid root node."))))))

(deftest test-node-valid?
  (testing "Node validation"  
    (testing "valid nodes"
     (is (valid? (Node. "blah" nil nil nil)))
     (is (valid? (Node. nil "balh" nil nil)))
     (is (valid? (Node. nil nil :test nil)))
     (is (valid? (Node. nil nil :node {:name "blah"}))))

    (testing "invalid nodes"
      (is (false? (valid? (Node. nil nil nil nil))))
      (is (false? (valid? (Node. "test" nil :test nil))))
      (is (false? (valid? (Node. nil "blah" :test nil))))
      (is (false? (valid? (Node. "balh" "test" nil nil))))
      (is (false? (valid? (Node. nil nil "element" nil))))
      (is (false? (valid? (Node. "test" nil nil {:attr "blah"}))))
      (is (false? (valid? (Node. nil "blah" nil {:attr "blah"}))))
      (is (false? (valid? (Node. nil nil nil {:attr "blah"}))))
      (is (false? (valid? (Node. nil nil :test [:attr "bla"]))))
      (is (false? (valid? (Node. nil nil :element "attributes")))))
    )) ; end test-node-valid?

(deftest test-dom-root
  (testing "DOM creation and root node."
   (let [n1 (Node. nil nil :test nil)
	 n2 (Node. "comment" nil nil nil)
	 n3 (Node. nil "text" nil nil)]

     (testing "root node association" 
       (is (= n1 (:root (create-dom n1))))
       (is (belongs? (create-dom n1) n1)))
     
     (testing "illegal root nodes"
       (is (thrown? Exception (create-dom n2)))
       (is (thrown? Exception (create-dom n3)))
       (is (thrown? Exception (create-dom nil))))
     ))) ; end test-dom-root

(deftest test-add-child
  (testing "add-child function"
   (let [n1 (Node. nil nil :root nil)
	 n2 (Node. nil nil :n2 nil)
	 n3 (Node. nil nil :n3 nil)
	 n4 (Node. "comment" nil nil nil)
	 n5 (Node. nil "text" nil nil)]
     
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
     ))) ; end test-add-child
     
(deftest test-insert-after
  (testing "Testing insert-after function"
    (let [n1 (Node. nil nil :root nil)
	  n2 (Node. nil nil :n2 nil)
	  n3 (Node. nil nil :n3 nil)
	  n4 (Node. "comment" nil nil nil)
	  n5 (Node. nil "text" nil nil)]

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
      ))) ; end test-insert-after 
