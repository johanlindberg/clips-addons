
;;;; Unit test functions for Clips - Johan Lindberg (johan@pulp.se)

;;;; 2009-05-05         Changed to BSD license.
;;;;
;;;; Copyright (c) 2007 - 2009, Johan Lindberg - Pulp Software
;;;; All rights reserved.
;;;; 
;;;; Redistribution and use in source and binary forms, with or
;;;; without modification, are permitted provided that the following 
;;;; conditions are met:
;;;; 
;;;;     * Redistributions of source code must retain the above
;;;;       copyright notice, this list of conditions and the following
;;;;       disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;;       copyright notice, this list of conditions and the following
;;;;       disclaimer in the documentation and/or other materials
;;;;       provided with the distribution.
;;;;     * Neither the name of Pulp Software nor the names of its
;;;;       contributors may be used to endorse or promote products
;;;;       derived from this software without specific prior written
;;;;       permission.
;;;;
;;;;       THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;       CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;       INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;       MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;       DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;       CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;;;       INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;;       (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;;       GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;;;       BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;;       LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;;       (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
;;;;       OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;;       POSSIBILITY OF SUCH DAMAGE.

(deffunction test-suite (?stream ?name $?test-cases)
   (printout ?stream crlf "Summary of test-suite: " ?name crlf)
   (progn$ (?test-case $?test-cases)
      (printout ?stream ?test-case crlf)))
      
(deffunction test-case (?name $?tests)
   (bind ?number-of-tests 0)
   (bind ?number-of-failed-tests 0)
   (bind ?messages (create$))
   
   (progn$ (?test $?tests)
      (if (eq ?test nil)
       then
         (bind ?number-of-tests (+ ?number-of-tests 1))
       else
         (bind ?number-of-tests (+ ?number-of-tests 1))
         (bind ?messages (insert$ ?messages (+ (length$ ?messages) 1) (str-cat "Test #" ?number-of-tests " failed: " ?test)))
         (bind ?number-of-failed-tests (+ ?number-of-failed-tests 1))))
   (return (create$ (str-cat ?number-of-failed-tests " of "
                             ?number-of-tests " unit tests failed in " ?name)
                    ?messages)))
                    
(deffunction test (?cmp ?result ?expected-result)
   (if (eq (type ?result) MULTIFIELD)
    then (bind ?r (str-cat "(create$ " (implode$ ?result) ")"))
    else (bind ?r ?result))
    
   (if (eq (type ?expected-result) MULTIFIELD)
    then (bind ?er (str-cat "(create$ " (implode$ ?expected-result) ")"))
    else (bind ?er ?expected-result))
   
   (if (eval (str-cat "(" ?cmp " " ?r " " ?er ")"))
    then (return nil)
    else 
      (if (eq (type ?result) MULTIFIELD)
       then (bind ?result (str-cat "(" (implode$ ?result) ")")))
       
      (if (eq (type ?expected-result) MULTIFIELD)
       then (bind ?expected-result (str-cat "(" (implode$ ?expected-result) ")")))
       
      (return (str-cat "(" ?cmp " " ?result " " ?expected-result ") -> FALSE"))))
      