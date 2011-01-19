(ns clojure-dom.test.xml.reader
  (:use [clojure-dom.xml.reader] :reload-all)
  (:use [clojure-dom node dom])
  (:use [clojure.test]))

(def xml-source "
<!--Header-->
<root a=\"blah\" b=\"1\">    \n<!-- comment one -->
<one>
   blahs  dfsf  asd ad


asdasd
sdsa

</one>
<two a=\"2\">
fsfs <!-- comment 2 -->
  <one/>
  <two> asd &lt;two </two>
  <three b=\"2\" c=\"\">    &#160; </three>
</two>
</root>
<!--Footer-->")

(deftest test-dom-structure
  (let [dom (parse-string xml-source)
	n0 (:root dom)
	n1 (first-child dom n0) ; comment
	n2 (next-sibling dom n1) ; one
	n3 (next-sibling dom n2) ; two
	]
    (testing "root node"
      (is (element? (:root dom)))
      (is (= :root (element (:root dom))))
      (is (= "blah" (:a (attributes (:root dom)))))
      (is (= "1" (:b (attributes (:root dom)))))
      (is (= 2 (count (attributes (:root dom)))))
      )
    (testing "DOM header and footer"
      (is (= "Header" (:header dom)))
      (is (= "Footer" (:footer dom)))
      )
    (testing "Immediate children of root"
      (is (comment? n1))
      (is (= :one (element n2)))
      (is (nil? (attributes n2)))
      (is (= :two (element n3)))
      (is (= "2" (attribute n3 :a)))
      (is (= 3 (count (child-nodes dom n0))))
      (is (= n3 (last-child dom n0)))
      )
    (let [n31 (first-child dom n3) ; text
	  n32 (next-sibling dom n31) ; comment
	  n33 (next-sibling dom n32) ; <one>
	  n34 (next-sibling dom n33) ; <two>
	  n35 (next-sibling dom n34) ; <three>
	  ]
      (testing "Nested child nodes"
	(is (text? n31))
	(is (= 5 (count (child-nodes dom n3))))
	(is (comment? n32))
	(is (= :one (element n33)))
	(is (nil? (attributes n33)))
	(is (nil? (child-nodes dom n33)))
	(is (= :two (element n34)))
	(is (nil? (attributes n34)))
	(is (= 1 (count (child-nodes dom n34))))
	(is (text? (last-child dom n34)))
	(is (element? n35))
	(is (= "2" (attribute n35 :b)))
	(is (= "" (attribute n35 :c))))
      (testing "Internal entity resolutions"
	(is (= "asd <two" (.trim (text (last-child dom n34)))))
	(is (= 1 (.length (.trim (text (first-child dom n35)))))) ; &#160;
	)  
      ))) ; end test-dom-structure