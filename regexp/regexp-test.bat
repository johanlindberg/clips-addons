;;;; Test suite for regexp.clp
(clear)

(load "unittest.clp")
(load "regexp.clp")

(defglobal ?*errors* = t)
(defglobal ?*unittest* = t)

(test-suite ?*unittest*
   "regexp.clp"
   
   (test-case "tokenize-expression"
      (test eq (tokenize-expression "a") (create$ "a"))
      (test eq (tokenize-expression "abc*") (create$ "abc" "*"))
      (test eq (tokenize-expression ".bc*") (create$ "." "bc" "*"))
      (test eq (tokenize-expression "^abc") (create$ "^" "abc"))
      (test eq (tokenize-expression "abc$") (create$ "abc" "$"))
      (test eq (tokenize-expression "^abc$") (create$ "^" "abc" "$"))
      (test eq (tokenize-expression "^.*$") (create$ "^" "." "*" "$")))
      
   (test-case "match"
      (test eq (match "a" "abc") "a")
      (test eq (match "abc" "abc") "abc")
      (test eq (match "abc$" "abc") "abc")
      (test eq (match "^abc" "abc") "abc")
      (test eq (match ".bc" "abc") "abc")
      (test eq (match "a.c" "abc") "abc")
      (test eq (match "ab.*" "abc") "abc")
      (test eq (match "ab.*c" "abc") "abc")
      (test eq (match "^.b*.$" "abc") "abc")
      (test eq (match ".*whatever*.*" "abc") "abc")
      (test eq (match "abc" "def") nil))
      
   (test-case "search"
      (test eq (search "abc" "fooabc") (create$ 4 6 "abc"))
      (test eq (search "abc$" "fooabc") (create$ 4 6 "abc"))
      (test eq (search "^f.." "fooabc") (create$ 1 3 "foo"))
      (test eq (search "a" "abcfoo") (create$ 1 1 "a"))
      (test eq (search "a" "fooabc") (create$ 4 4 "a"))
      (test eq (search "b." "fooabc") (create$ 5 6 "bc"))
      (test eq (search "a" "fooa") (create$ 4 4 "a"))
      (test eq (search "c" "fooa") (create$ -1 -1 nil)))
      
   (test-case "findall"
      (test eq (findall "a" "fooafoobfooafooa") (create$ (implode$ (create$ 4 4 "a"))
                                                         (implode$ (create$ 12 12 "a"))
                                                         (implode$ (create$ 16 16 "a"))))
      (test eq (findall "foo" "fooafoobfooafooa") (create$ (implode$ (create$ 1 3 "foo"))
                                                         (implode$ (create$ 5 7 "foo"))
                                                         (implode$ (create$ 9 11 "foo"))
                                                         (implode$ (create$ 13 15 "foo"))))
      (test eq (findall "oo." "fooafoobfooafooa") (create$ (implode$ (create$ 2 4 "ooa"))
                                                         (implode$ (create$ 6 8 "oob"))
                                                         (implode$ (create$ 10 12 "ooa"))
                                                         (implode$ (create$ 14 16 "ooa"))))
      (test eq (findall "bar" "fooafoobfooafooa") (create$)))
      
   (test-case "split"
      (test eq (split "a" "fooafoobfooafooa") (create$ "foo" "foobfoo" "foo" ""))
      (test eq (split "a" "fooafoobfooafooa" 1) (create$ "foo" "foobfooafooa"))
      (test eq (split "a" "fooafoobfooafooa" 2) (create$ "foo" "foobfoo" "fooa"))
      (test eq (split "o" "fooafoobfooafooa") (create$ "f" "af" "bf" "af" "a")))
      
   (test-case "sub(stitute)"
      (test eq (sub "a" "*" "fooafoobfooafooa") "foo*foobfoo*foo*")
      (test eq (sub "a" "*" "fooafoobfooafooa" 2) "foo*foobfoo*fooa")
      (test eq (sub "o" "" "fooafoobfooafooa") "fafbfafa")))
