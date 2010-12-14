(ns clojure-dom.xml.reader
  (:use clojure-dom.core)
  (:gen-class
   :implements [org.xml.sax.helpers.DefaultHandler org.xml.sax.ext.LexicalHandler]
   :init init
   :constructors {[]}
   :state state))

(defn map-atts
  "Creates a map out of attributes names and values" 
  [#^org.xml.sax.Attributes atts]
  (if (= (.getLength atts) 0)
    {}
    (reduce merge
	    (for [idx (range (.getLength atts))]
	      (assoc {} (to-keyword (.getQName atts idx)) (.getValue atts idx))))))

(defn -init [] ; class constructor
  [[] (ref nil)]) ; state will be used for building DOM


;; DefaultHandler methods
(defn -startDocument [this]
					; do nothing
  )

(defn -endDocument [this]
					; do nothing
  )

(defn  -startElement [this,
		     uri,
		     localName,
		     qName,
		      #^org.xml.sax.Attributes attributes]
					;TODO
  )

(defn  -endElement [this,
		     uri,
		     localName,
		     qName]
					;TODO
  )

(defn -characters [ this,
		   ^chars ch,
		   ^int start,
		   ^int length]
					;TODO
  )


(defn -ignorableWhitespace [ this,
			    ^chars ch,
			    start,
			    length]
; TODO
  )

(defn -warning [this, e]
  ; TODO
  )

(defn -error [this, e]
  ; TODO
  )
(defn -fatalError [this, e]
  ; TODO
  )

;; LexicalHandler methods
(defn -comment [this,
		^chars ch,
		start,
		length]
					;TODO
  )

(defn -startEntity [ this, ^String name]
  ;TODO
  )

(defn -endEntity [ this, #^String name]
  ;TODO
)

(defn -startCDATA [this]
; do nothing
  )

(defn -endCDATA [this]
; do nothing
  )