(ns clojure-dom.node
  (:use [clojure-dom.common]))

					; Node types
(def COMMENT :comment)
(def TEXT :text)
(def ELEMENT :element)
(def XML-NODES #{COMMENT TEXT ELEMENT})
					; end Node types

(defn valid-attributes?
  "Returns true if attribute map is valid - i.e. a map with keyword keys"
  [am]
  (and
   (map? am)
   (not (empty? am)) ; should use nil instead of empty map
   (every? keyword? (keys am)))) ; note that the value of the attribute can be anything


(defprotocol NodeValidation
  "Functions that test node and dom compatibility with XML"
  (xml-valid? [x] "Returns true if x is valid. x can be either a Node or Dom."))

(defprotocol NodeAccessors
  "Reading various Node parameters"
  (comment? [this] "Returns true if node is of the COMMENT type")
  (text? [this] "Returns true if node is of the TEXT type")
  (element? [this] "Returns true if node is of the ELEMENT type")

  (comment-text [this] "Returns comment text if node is of the comment type, otherwise nil")
  (text [this] "Returns the text content of the node if the node is of the text type, otherwise nil")
  (element [this] "Returns an element name as a keyword, if node is of an element type, otherwise nil")
  (value [this] "Returns node value. The value will differ depending on the node type - i.e. sting for TEXT and COMMENT,
keyword for ELEMENT")
  
  (attributes [this] "Returns an attributes map if node has attributes, otherwise nil")
  (attribute [this attr] "Returns a value of the attribute, or nil of none")
  ) ; end NodeAccessors

(defprotocol NodeModification
  "Functions that provide 'modification' of the node parameters. Note that the original node is not affected - a new node is created with modified paramateres."
  (delete-attr [this & attrs] "Returns a new element node with the specified attributes removed from the attributes map.")
  ; change-attr, 
  )

(deftype Node
    [_type _value _attributes]
  java.lang.Cloneable
  (clone [this] (Node. _type _value _attributes))
  
  NodeAccessors
  (comment? [this] (= _type COMMENT))
  (text? [this] (= _type TEXT))
  (element? [this] (= _type ELEMENT))
  
  (comment-text [this] (when (comment? this) _value))
  (text [this] (when (text? this) _value))
  (element [this] (when (element? this) _value))
  (value [this] _value)
  (attributes [this] _attributes)
  (attribute [this attr] (get _attributes attr))

  NodeValidation
  (xml-valid? [this]
	      
	      (cond
	       
	       (comment? this)
	       (and (string? _value) (nil? _attributes))
	       
	       (text? this)
	       (and (string? _value) (nil? _attributes))

	       (element? this)
	       (and (keyword? _value) (or (nil? _attributes) (valid-attributes? _attributes)))

	       :default
	       false
	       )) ; end xml-valid?

  NodeModification
  (delete-attr [this & attrs]
	       (let [ at-map (apply dissoc _attributes attrs)]
		 (new Node _type _value (if (empty? at-map) nil at-map))))

  ) ; end Node


(defn node?
  "Returns true if x is an instance of the Node type"
  [x]
  (instance? Node x))
     

(defn create-node
  "Creates a new Node. Note that for XML-compatible nodes it is best to use text-node, comment-node and element-node functions."
  [type value attrs]
  (Node. type value attrs))

(defn text-node
  "Returns a text node that contains the given string. "
  [s]
  (Node. TEXT (str s)  nil))

(defn comment-node
  "Returns a comment node that contains the given string."
  [s]
  (Node. COMMENT (str s) nil))

(defn element-node
  "Returns an element node based on the. el is the element name as a keyword or a string. at-map is a map of the attributes.If the resulting element node is not valid, an exception is thrown."
  ([el] (element-node el nil))
  ([el at-map]
     (let [k (if (keyword? el) el (to-keyword el))
	   am (if (empty? at-map) nil at-map)
	   n (Node. ELEMENT k am)]
       (if (xml-valid? n)
	 n
	 (throw (Exception. (str "Invalid element node " n))))))
  ([el atr val & keyvals] (element-node el (conj (hash-map atr val) (apply hash-map keyvals)))))
