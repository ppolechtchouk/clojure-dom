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



(defprotocol NodeAccessors
  "Reading various Node parameters"
  (comment-text [this] "Returns comment text if node is of the comment type, otherwise nil")
  (text [this] "Returns the text content of the node if the node is of the text type, otherwise nil")
  (element [this] "Returns an element name as a keyword, if node is of an element type, otherwise nil")
  (attributes [this] "Returns an attributes map if node has attributes, otherwise nil")
  (attribute [this attr] "Returns a value of the attribute, or nil of none")
  ) ; end NodeAccessors

(defprotocol AttributeManagement
  "Functions that provide 'modification' of the attributes of the element nodes."
  (delete-attr [this & attrs] "Returns a new element node with the specified attributes removed from the attributes map.")
  ; change-attr, 
  )

(deftype Node
    [com txt el attrs]
  Validation
  (valid? [this]
	  (not
	   (cond
	    
	    com
	    (or txt el attrs (not (string? com)))
	    
	    txt
	    (or com el attrs (not (string? txt)))

	    attrs ; plus element
	    (or (not el) txt com (not (verify-attributes attrs)))
	    
	    el ; no attributes
	    (or com txt (not (keyword? el)))

	    :default
	    true ; i.e. not valid
	    ))) ; end valid?
  NodeAccessors
  (comment-text [this] com)
  (text [this] txt)
  (element [this] el)
  (attributes [this] attrs)
  (attribute [this attr] (get attrs attr))

  AttributeManagement
  (delete-attr [this & ats]
	       (let [ at-map (apply dissoc attrs ats)]
		 (Node. com txt el (if (empty? at-map) nil at-map))))
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
  
  (mutate-node [dom n1 n2] "Returns a new dom with node n1 replaced by a copy of node n2. Instead of using a Node for :text and :comment nodes you can use key-value pairs that describe the node. For example :text 'blah' instead of a text node. n1 must belong to dom. If n1 had children, they will become the children of n2. Note that the n1 and n2 must be of the same type, i.e. you can only mutate text into other text, comment into other comment, otherwise an exception will be thrown. To replace a node by another node type use replace-node. Since n2 will be copied and not used directly, n2 can belong to dom.")
  )

(defprotocol DomComments
  "Functions for supporting header and footer comments - i.e. a comment immediately before and a comment immediately after a root node"
  (add-header [dom s] "Returns a dom with the string s as a header comment. Use nil to remove existing comment")
  (add-footer [dom s] "Returns a dom with the string s as a footer comment. Use nil to remove existing comment")
  (add-comments [dom header footer] "Returns a dom with header and footer comments")
  )

(defprotocol DomExport
  "Functions that allow exporting a sub-section of a DOM as a new DOM"
  (exported-nodes-set [dom n] "Returns a set of nodes that would be exported or nil in node n can not be exported.")
  (export-dom [dom n] "Returns a new DOM with node n as the root. The DOM will contain all the sub-structure unter n. If there are comment nodes around n, they will be used as header and footer source.
Throws an exception is n is an illegal root node or n does not belong to dom. ")
 )

(defn verify-parent
  "Returns true if n is a valid parent node - i.e. element node and belongs to the DOM"
  [dom n]
  (and
   (= (class n) Node)
   (element n)
   (belongs? dom n)))

 

(defrecord Dom 
    [root ; root node. Must be an element node
     parent-map ; hash map in the form of {node parent-node}
     first-child-map ; hash map {parent-node first-child-node}
     last-child-map ; hash map {parent-node last-child-node}
     previous-sibling-map ; hash map {node previous-node}
     next-sibling-map ; hash map {node next-node}
     nodes-set ; a set of all nodes belonging to this DOM
     header ; comment text that is placed before the root node
     footer ; comment text that is placed after the root node
     ]
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
  DomComments
  (add-header [dom s] (assoc dom :header (if s (str s) nil)))
  (add-footer [dom s] (assoc dom :footer (if s (str s) nil)))
  (add-comments [dom h f]
		(assoc dom :header (if h (str h) nil) :footer (if f (str f) nil)))
  

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
		      nodes-set
		      header
		      footer
		      ))
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
		      (:header odom)
		      (:footer odom)
		      ))
	       )) ; end add-child
   (mutate-node [dom n1 n2]
		nil);TODO
  ;TODO
   
   DomExport
   (exported-nodes-set [dom n]
		       (if (and (belongs? dom n) (element n))
			 (loop [children (child-nodes dom n)
				result (list n)
				]
			   (if (empty? children)
			     (set result)
			     (recur
			      (mapcat #(child-nodes dom %) children)
			      (concat result children))
			     )))) ; end exported-nodes-set
   (export-dom [dom n]
	       (cond
		; Perform checks
		(not (belongs? dom n))
		(throw (Exception. (str "Trying to export a node " n " that does not belong to DOM.")))

		(or (comment-text n) (text n))
		(throw (Exception. (str n " is an illegal root node.")))

		;all OK
		:default
		(let [ns (exported-nodes-set dom n)
		      nsn (apply disj nodes-set ns)]
		 (new Dom
		      n
		      (dissoc (apply dissoc parent-map nsn) n)
		      (apply dissoc first-child-map nsn) ; hash map {parent-node first-child-node}
		      (apply dissoc last-child-map nsn) ; hash map {parent-node last-child-node}
		      (dissoc (apply dissoc previous-sibling-map nsn) n) ; hash map {node previous-node}
		      (dissoc (apply dissoc next-sibling-map nsn) n) ; hash map {node next-node}		      
		      ns
		      (comment-text (previous-sibling dom n))
		      (comment-text (next-sibling dom n))))
		))
   ) ; end Dom



(defn validate-root
  "Returns true if root node is valid"
  [n]
  (and (= (class n) Node)
       (element n)))

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
	 nil ;header
	 nil ;footer
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




     

