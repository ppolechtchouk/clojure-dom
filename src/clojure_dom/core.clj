(ns clojure-dom.core
  (:use clojure.test))

(def *keywords* (agent 
		 (hash-map)
		 :meta {:doc "Hash-map of {string :string} used for keyword creation optimisation"}))

(defn update-keywords
  "Adds a new keyword to the *keywords* map, based on the value of string s. Returns a keyword based on s"
  [s]
  (let [k (keyword s)]
   (send *keywords* assoc s k)
   k))

(defn to-keyword
  "Optimised version of the keyword function that uses *keywords*. Returns an :s keyword"
  [s]
  (let [kw (get @*keywords* s)]
    (if kw
      kw
      (update-keywords s))))

(declare text-node comment-node element-node)

(defprotocol Validation
  "Functions that test node and dom internal consistency"
  (valid? [x] "Returns true if x is valid. x can be either a Node or Dom."))

(defn verify-attributes
  "Returns true if attribute map is valid - i.e. a map with keyword keys"
  [am]
  (and
   (map? am)
   (not (empty? am)) ; should use nil instead of empty map
   (every? keyword? (keys am)))) ; note that the value of the attribute can be anything

(defprotocol AttributeManagement
  "Functions that provide access and 'modification' of the attributes of the element nodes."
  (attribute [this attr] "Returns the value of the attr attribute or nil if none.")
  (delete-attr [this & attrs] "Returns a new element node with the specified attributes removed from the attributes map.")
  ; change-attr, 
  )

(defrecord Node
    [comment text element attributes]
  Validation
  (valid? [this]
	  (not
	   (cond
	    
	    comment
	    (or text element attributes (not (string? comment)))
	    
	    text
	    (or comment element attributes (not (string? text)))

	    attributes ; plus element
	    (or (not element) text comment (not (verify-attributes attributes)))
	    
	    element ; no attributes
	    (or comment text (not (keyword? element)))

	    :default
	    true ; i.e. not valid
	    ))) ; end valid?

  AttributeManagement
  (attribute [this attr] (get attributes attr))

  (delete-attr [this & attrs]
	       (let [ at-map (apply dissoc attributes attrs)]
		 (Node. comment text element (if (empty? at-map) nil at-map))))
  ) ; end Node

(defprotocol DomAccess
  "Various functions for accessing DOM structure."
  (belongs? [dom n] "Returns true if node is present in the dom.")
  (parent [dom n] "Returns the parent node of n or nil if none")
  (parent-nodes [dom n] "Returns a sequence of parent nodes of n, starting from the immediate parent and all the way up to the root node")
  (ancestor? [dom n1 n2] "Returns true if n1 is one of the parent nodes of n2.")
  (next-sibling [dom n] "Retuns the next sibling node or nil if none")
  (previous-sibling [dom n] "Retuns the next sibling node or nil if none")
  (first-child [dom n] "Returns the first child of the node, or nil if none")
  (last-child [dom n] "Returns the last child of the node, or nil if none")
  (child-nodes [dom n] "Returns a sequence of the child nodes.")
  )

(defprotocol DomModification
  "Various functions for creating a new DOM instance with a modified structure."

  (add-child [dom np nc] "Returns a new DOM with the child node (nc) added to the parent node (np). Note that nc will be the last child of np if np already has children. Throws an exception if np is not a valid parent node, or nc is not a valid child node. If nc belongs to this dom, 'add-child-with-move' is executed instead.")
  (add-child-with-move [dom np nc] "Returns a new DOM with the child node (nc) added to the parent node (np). If nc belongs to the dom and has any children, they will be moved as well. Throws an exception if np is not a valid parent node or nc is not a valid child. Note that if nc does not belong to dom, using 'add-child' will be quicker.")
  (insert-after [dom n1 n2] "Returns a new DOM with n2 as the next sibling of n1. If n2 is already part of the DOM structure, it will be moved to a new location. n1 can not be a root node and must belong to the DOM otherwise an exception is thrown. n2 can not be one of the parent nodes of n1.")
)

(defn verify-parent
  "Returns true if n is a valid parent node - i.e. element node and belongs to the DOM"
  [dom n]
  (and
   (= (class n) Node)
   (:element n)
   (belongs? dom n)))

(defn orphanize
  "Returns a dom with the node n removed from first-child, last-child, previous-sibling and next-sibling maps. Note that the resulting structure is illegal. This function is used as an intermediate only!"
  [dom n]
  (let [ps (previous-sibling dom n)
	ns (next-sibling dom n)
	np (parent dom n)]
    (new Dom
	(:root dom)
	(:parent-map dom)
	(if (= n (first-child dom np))
	  (assoc (:first-child-map dom) np ns)
	  (:first-child-map dom))
	(if (= n (last-child dom np))
	  (assoc (:last-child-map dom) np ps)
	  (:last-child-map dom))
	(assoc (dissoc (:previous-sibling-map dom) n) ns ps)
	(assoc (dissoc (:next-sibling-map dom) n) ps ns) 
	(:nodes-set dom)))) ; end orphanize

(defrecord Dom 
    [root parent-map first-child-map last-child-map previous-sibling-map next-sibling-map nodes-set]
  DomAccess
  (belongs? [dom n]
	    (contains? nodes-set n))
  (parent [dom n] (parent-map n))
  (parent-nodes [dom n]
		(take-while #(not (nil? %)) (iterate parent-map n)))
  (ancestor? [dom n1 n2]
	     (contains? (set (parent-nodes dom n2)) n1))
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
					; move nc structure
	       (belongs? this nc)
	       (add-child-with-move this np nc)

					; check parent node
	       (not (verify-parent this np))
	       (throw (Exception. (str np " is not a valid parent node")))

					;check child node
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

   (add-child-with-move [dom np nc]
     (cond

      (not (belongs? dom nc))
      (add-child dom np nc)
					; 
      (= np nc)
      (throw (Exception. (str "Trying to add " nc " as a child of itself")))

      (ancestor? dom nc np)
      (throw (Exception. (str nc " is an ancestor of " np)))

      (not (instance? Node nc))
      (throw (Exception. (str nc " is not a valid child node")))

      :default ; np and nc OK
      (let [fc (first-child-map np)
	    lc (last-child-map np)
	    odom (orphanize dom nc)]
	(new Dom
	     root			     ; root
	     (assoc parent-map nc np)     ; parent
	     (if fc
	       (:first-child-map odom)
	       (assoc (:first-child-map odom) np nc)) ; first child
	     (assoc (:last-child-map odom) np nc)	       ; last child
	     (assoc (:previous-sibling-map odom) nc lc) ; prev sibling
	     (assoc (:next-sibling-map odom) lc nc) ; next-sibling
	     nodes-set 	       ; nodes
	     ))
      )) ; end add-child-with-move
   
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

(defn text-node
  "Returns a text node that contains the given string. "
  [s]
  (Node. nil (str s)  nil nil))

(defn comment-node
  "Returns a comment node that contains the given string."
  [s]
  (Node. (str s) nil nil nil))

(defn element-node
  "Returns an element node based on the. el is the element name as a keyword or a string. at-map is a map of the attributes.If the resulting element node is not valid, an exception is thrown."
  ([el] (element-node el nil))
  ([el at-map]
     (let [k (if (keyword? el) el (to-keyword el))
	   am (if (empty? at-map) nil at-map)
	   n (Node. nil nil k am)]
       (if (valid? n)
	 n
	 (throw (Exception. (str "Invalid element node " n))))))
  ([el atr val & keyvals] (element-node el (conj (hash-map atr val) (apply hash-map keyvals)))))




     

