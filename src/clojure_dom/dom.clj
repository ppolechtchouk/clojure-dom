(ns clojure-dom.dom
  (:use [clojure-dom common node]))


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
  (child-nodes [dom n] "Returns a sequence of the child nodes or nil if node has no children")
  )

(defprotocol DomModification
  "Various functions for creating a new DOM instance with a modified structure."
  (orphanize [dom n]  "Returns a dom with the node n removed from first-child, last-child, previous-sibling and next-sibling maps. Note that the resulting structure is illegal. This function is used as an intermediate only! If n does not belong to dom, the original dom is returned.")
  (add-child [dom np nc] "Returns a new DOM with the child node (nc) added to the parent node (np). Note that nc will be the last child of np if np already has children. If nc belongs to the dom and has any children, they will be moved as well. Throws an exception if np is not a valid parent node, or nc is not a valid child node.")
 
					; (insert-before [dom n1 n2] "Returns a new DOM with n2 as the next sibling of n1. If n2 is already part of the DOM structure, it will be moved to a new location. n1 can not be a root node and must belong to the DOM otherwise an exception is thrown. n2 can not be one of the parent nodes of n1.")
  
  (mutate-node [dom n1 n2] "Returns a new dom with node n1 replaced by n2. n1 must belong to the dom, and n2 can not belong to the dom, otherwise an IllegalArgumentException is thrown. If n1 had children, they will become the children of n2. ")
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
	       (when (first-child-map n)
		 (take-while #(not (nil? %)) (iterate next-sibling-map (first-child-map n)))))
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
	       (not (belongs? this np))
	       (throw (IllegalArgumentException. (str np " is not a part of the DOM")))

					;check child node
	       (not (node? nc))
	       (throw (IllegalArgumentException. (str nc " is not a Node")))

	       (= np nc)
	       (throw (IllegalArgumentException. (str "Trying to add " nc " as a child of itself")))

	       (ancestor? this nc np)
	       (throw (IllegalArgumentException. (str nc " is an ancestor of " np)))

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
		(cond
		 ;checks
		 (not (belongs? dom n1))
		 (throw (IllegalArgumentException. (str n1 " does not belong to the DOM")))

		 (not (node? n2))
		 (throw (IllegalArgumentException. (str n2 " is not a Node")))
		 
		 (belongs? dom n2)
		 (throw (IllegalArgumentException. (str n2 " belongs to the DOM")))

		 :default ; checks passed
		 (let [children (child-nodes dom n1)
		       p (parent dom n1)
		       ps (previous-sibling dom n1)
		       ns (next-sibling dom n1)
		       fc (first-child dom n1)
		       lc (last-child dom n1)]
		   (Dom.
					; root node
		    (if (= n1 root) n2 root)
					; parent-map
		    (dissoc
		     (assoc (reduce (fn [m c] (assoc m c n2)) parent-map children) n2 p)
		     n1)
					; first-child-map 
		    (dissoc
		     (if (= n1 (first-child dom p))
		       (assoc first-child-map p n2 , n2 fc)
		       (assoc first-child-map n2 fc))
		     n1)
					; last-child-map  
		    (dissoc
		     (if (= n1 (last-child dom p))
		       (assoc last-child-map p n2 , n2 lc )
		       (assoc last-child-map n2 lc))
		     n1)
					; previous-sibling-map
		    (dissoc (assoc previous-sibling-map ns n2, n2 ps) n1 nil)
					; next-sibling-map
		    (dissoc (assoc next-sibling-map ps n2, n2 ns) n1 nil)

		    (disj (conj nodes-set n2) n1)
	    
		   header		       ;header
		   footer		       ;footer
		   ))
		 
		 ));TODO
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

		:default ; all checks passed
		(let [ns (exported-nodes-set dom n)
		      nsn (apply disj nodes-set ns)
		      prevs (previous-sibling dom n)
		      nexts (next-sibling dom n)]
		  (new Dom
		       n
		       (dissoc (apply dissoc parent-map nsn) n)
		       (apply dissoc first-child-map nsn) ; hash map {parent-node first-child-node}
		       (apply dissoc last-child-map nsn) ; hash map {parent-node last-child-node}
		       (dissoc (apply dissoc previous-sibling-map nsn) n) ; hash map {node previous-node}
		       (dissoc (apply dissoc next-sibling-map nsn) n) ; hash map {node next-node}
		       ns
		       (when (comment? prevs) (comment-text prevs))
		       (when (comment? nexts) (comment-text nexts))))		
		)) ; end export-dom
   ) ; end Dom

(defn dom?
  "Returns true if x is an instance of Dom"
  [x]
  (instance? Dom x))

(defn create-dom
  "Creates a DOM ready to be filled with nodes. If you do not supply a root node, a dummy node will be created."
  ([] (create-dom (element-node :root)))
  ([n]
   (if (node? n)
    (Dom.
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


     

