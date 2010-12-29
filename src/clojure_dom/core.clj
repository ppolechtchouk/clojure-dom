(ns clojure-dom.core
  )

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
  (orphanize [dom n]  "Returns a dom with the node n removed from first-child, last-child, previous-sibling and next-sibling maps. Note that the resulting structure is illegal. This function is used as an intermediate only! If n does not belong to dom, the original dom is returned.")
  (add-child [dom np nc] "Returns a new DOM with the child node (nc) added to the parent node (np). Note that nc will be the last child of np if np already has children. If nc belongs to the dom and has any children, they will be moved as well. Throws an exception if np is not a valid parent node, or nc is not a valid child node.")
 
 ; (insert-before [dom n1 n2] "Returns a new DOM with n2 as the next sibling of n1. If n2 is already part of the DOM structure, it will be moved to a new location. n1 can not be a root node and must belong to the DOM otherwise an exception is thrown. n2 can not be one of the parent nodes of n1.")

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
  ; note that a root node can have comment nodes as previous or next siblings
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
  (orphanize  [dom n]
	      (if (belongs? dom n)
	       (let [ps (previous-sibling dom n)
		     ns (next-sibling dom n)
		     np (parent dom n)]
		 (new Dom
		      root
		      (dissoc parent-map n)
		      (if (= n (first-child dom np))
			(assoc first-child-map np ns)
			first-child-map)
		      (if (= n (last-child dom np))
			(assoc last-child-map np ps)
			last-child-map)
		      (assoc (dissoc previous-sibling-map n) ns ps)
		      (assoc (dissoc next-sibling-map n) ps ns) 
		      nodes-set))
	       dom ; no need for modification
	       )) ; end orphanize
   (add-child [this np nc]
	      (cond
					; check parent node
	       (not (verify-parent this np))
	       (throw (Exception. (str np " is not a valid parent node")))

					;check child node
	       (not (instance? Node nc))
	       (throw (Exception. (str nc " is not a valid child node")))

	       (= np nc)
	       (throw (Exception. (str "Trying to add " nc " as a child of itself")))

	       (ancestor? this nc np)
	       (throw (Exception. (str nc " is an ancestor of " np)))

					; both np and nc are ok

	       (= nc  (last-child-map np)) ; no change if the node is not even moving
	       this

	       :default
	       (let [fc (first-child-map np)
		     lc (last-child-map np)
		     odom (orphanize this nc)]		 
		 (new Dom
		      root			     ; root
		      (assoc (:parent-map odom) nc np)   ; parent
		      (if fc (:first-child-map odom) ; first child - no change
			  (assoc (:first-child-map odom) np nc) ; change
			  )
		      (assoc (:last-child-map odom) np nc) ; last child
		      (assoc (:previous-sibling-map odom) nc lc) ; prev sibling
		      (if lc (assoc (:next-sibling-map odom) lc nc) ; next-sibling - changed
			  (:next-sibling-map odom) ; no change
			  )
		      (conj nodes-set nc)	       ; nodes
		      ))
	       )) ; end add-child
   ) ; end Dom



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




     

