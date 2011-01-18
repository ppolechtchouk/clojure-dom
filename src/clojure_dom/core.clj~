(ns clojure-dom.core
  (:use [clojure-dom common node dom]))


(defn create-node
  "Creates a new Node. Note that for XML-compatible nodes it is best to use text-node, comment-node and element-node functions."
  [type value attrs]
  (new clojure-dom.node.Node type value attrs))

(defn text-node
  "Returns a text node that contains the given string. "
  [s]
  (create-node TEXT (str s)  nil))

(defn comment-node
  "Returns a comment node that contains the given string."
  [s]
  (create-node COMMENT (str s) nil))

(defn element-node
  "Returns an element node based on the. el is the element name as a keyword or a string. at-map is a map of the attributes.If the resulting element node is not valid, an exception is thrown."
  ([el] (element-node el nil))
  ([el at-map]
     (let [k (if (keyword? el) el (to-keyword el))
	   am (if (empty? at-map) nil at-map)
	   n (create-node ELEMENT k am)]
       (if (xml-valid? n)
	 n
	 (throw (Exception. (str "Invalid element node " n))))))
  ([el atr val & keyvals] (element-node el (conj (hash-map atr val) (apply hash-map keyvals)))))


(defn create-dom
  "Creates a DOM ready to be filled with nodes. If you do not supply a root node, a dummy node will be created."
  ([] (create-dom (element-node :root)))
  ([n]
   (if (node? n)
    (new clojure-dom.dom.Dom
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




     

