
;;;; A small regexp library for Clips. Inspired by the first chapter of the
;;;; book 'Beautiful Code' (O'Reilly). Enjoy. - Johan Lindberg (johan@pulp.se)
;;;; 
;;;; 2007-10-04 .. 16   1st release. Implemented match and search
;;;; 2007-11-29 .. 30   2nd release. Implemented findall, split and sub(stitute)
;;;;                    also fixed a bug in search that sometimes caused the
;;;;                    ?string not to be properly scanned (ended to early).
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

(defglobal ?*errors* = t)

(deffunction tokenize-expression (?regexp)
  "tokenize-expression is used to split a regular expression pattern into
   a multifield variable that can be used in the match function."
   
  (bind ?string-index 1)
  (bind ?string-anchor 1)   ; ?string-anchor stores the ?string-index at which
                            ; the last token was matched. It is used to
                            ; retrieve character sequences (literal matches)
                            ; from ?regexp.
                            
  (bind ?tokens (create$))
  
  (while (< ?string-index (+ (str-length ?regexp) 1))
    (bind ?c (sub-string ?string-index ?string-index ?regexp))
    
    ;; Special characters: wildcard, quantifier and anchors
    (if (or (eq ?c ".")     ; matches any character
            
            (eq ?c "*")     ; match the previous token 0..n times
               
            (eq ?c "^")     ; matches the beginning of the string
            (eq ?c "$"))    ; matches the end of the string
     then
       ;; Check that anchors don't appear in strange places
       ;; of the regexp pattern.
       (if (or (and (eq ?c "^")
                    (> ?string-index 1))
               (and (eq ?c "$")
                    (< ?string-index (str-length ?regexp))))
        then
          (printout ?*errors* "; ERROR: Illegal Regular Expression Pattern." crlf)
          (printout ?*errors* "; You cannot use an anchor such as " ?c
                              " in the wrong place in a pattern!" crlf)
          (return nil))
          
       ;; Tokenize the sub string from anchor to index
       (if (< ?string-anchor ?string-index)
        then (bind ?tokens (insert$ ?tokens
                                    (+ (length$ ?tokens) 1)
                                    (sub-string ?string-anchor
                                                (- ?string-index 1)
                                                ?regexp))))
                                                
       ;; Check that quantifier tokens are not used with the
       ;; empty string.
       (if (and (eq ?c "*")
                (eq (length$ ?tokens) 0))
        then
          (printout ?*errors* "; ERROR: Illegal Regular Expression Pattern." crlf)
          (printout ?*errors* "; You cannot apply a quantifier such as " ?c
                              " on an empty string!" crlf)
          (return nil))
          
       ;; Tokenize the wildcard and anchor character
       (bind ?tokens (insert$ ?tokens
                              (+ (length$ ?tokens) 1)
                              (sub-string ?string-index
                                          ?string-index
                                          ?regexp)))
                                          
       (bind ?string-index (+ ?string-index 1))
       (bind ?string-anchor ?string-index)
     else
       (bind ?string-index (+ ?string-index 1))))
       
  ;; Tokenize the sub string from anchor to the end of the string
  (if (< ?string-anchor ?string-index)
   then (bind ?tokens (insert$ ?tokens
                               (+ (length$ ?tokens) 1)
                               (sub-string ?string-anchor
                                           (- ?string-index 1)
                                           ?regexp))))
  (return ?tokens))
  
  
(deffunction match (?pattern ?string)
  "match returns the string that ?pattern matched (in the *beginning* of
   ?string) otherwise it returns nil. If you want to match anywhere
   in ?string use search."
  
  (bind ?regexp (tokenize-expression ?pattern))
  (bind ?string-index 1)
  
  (progn$ (?part ?regexp)
    (bind ?prev-part (nth$ (- ?part-index 1) ?regexp))
    (bind ?next-part (nth$ (+ ?part-index 1) ?regexp))
    (bind ?next-token-index nil)
    
    ;; Match anchors. These are non-consuming (the ?string-index isn't
    ;; incremented).
    (if (and (eq ?part "^")
             (neq ?string-index 1))
     then (return nil))
     
    (if (and (eq ?part "$")
             (neq ?string-index (+ (str-length ?string) 1)))
     then (return nil))
     
    (if (neq ?next-part "*")
     then
       ;; Match a literal character (sequence)
       (if (and (neq ?part "^")
                (neq ?part "$")
                (neq ?part ".")
                (neq ?part "*"))
        then
          (if (neq ?part (sub-string ?string-index
                                     (- (+ (str-length ?part) ?string-index) 1)
                                     ?string))
           then (return nil)
           else (bind ?string-index (+ (str-length ?part) ?string-index))))
          
       ;; Match any character
       (if (eq ?part ".")
        then (bind ?string-index (+ ?string-index 1))))
        
       ;; Match the previous token 0..n times
       (if (eq ?part "*")
        then
          (if (and (neq ?next-part "$")
                   (neq ?next-part ".")
                   (neq ?next-part nil))
           then
             ;; Check for ?next-part in the remaining ?string
             (bind ?str-index (str-index ?next-part
                                         (sub-string ?string-index
                                                     (str-length ?string)
                                                     ?string)))
                                                     
             (if (eq ?str-index FALSE)
              then
                (bind ?next-token-index nil)
                (if (neq (nth$ (+ ?part-index 2) ?regexp) "*")
                 then
                   ;; If we can't find the next token and if it's
                   ;; not quantified the match fails!
                   (return nil))
              else
                (bind ?next-token-index (+ ?str-index ?string-index)))
                
           else
             (if (eq ?next-part ".")
              then (bind ?next-token-index nil) ; this can't be decided here!
              else (bind ?next-token-index (str-length ?string))))
             
             
          (if (neq ?next-token-index nil)
           then
             (if (eq ?prev-part ".")
              then
                ;; If the previous token is a wildcard we'll just increment
                ;; the ?string-index to wherever the next token begins.
                (bind ?string-index (- ?next-token-index 1))
                
              else
                ;; If the previous token is a literal character sequence
                ;; we'll first see if it's possible to fit 0..n pieces of
                ;; that sequence into the match.
                (bind ?match-length (- ?next-token-index ?string-index))
                (if (neq (mod ?match-length (str-length ?next-part)) 0)
                 then
                   ;; If it's not we can fail here!
                   (return nil)
                   
                 else
                   ;; Otherwise we must compare the remaining ?string
                   ;; character sequence by character sequence.
                   (while (< ?string-index ?next-token-index)
                      (if (neq ?prev-part (sub-string ?string-index
                                                      (- (+ (str-length ?prev-part) ?string-index) 1)
                                                      ?string))
                       then
                         (return nil)
                         
                       else
                         (bind ?string-index (+ ?string-index (str-length ?prev-part)))))))
           else
             ;; Match as much as possible (greedy)!
             (bind ?match true)
             (while (eq ?match true)
                (if (neq ?prev-part (sub-string ?string-index
                                               (- (+ (str-length ?prev-part) ?string-index) 1)
                                               ?string))
                 then
                   (bind ?match false)
                 else
                   (bind ?string-index (+ ?string-index (str-length ?prev-part))))))))
                   
  (if (and (neq ?next-token-index nil)
           (< ?string-index ?next-token-index))
   then (return (sub-string 1 ?next-token-index ?string))
   else (return (sub-string 1 (- ?string-index 1) ?string))))
  
  
(deffunction search (?pattern ?string)
  "find and return information about the first occurance of ?pattern in
   ?string. Returns a multifield consisting of start-index, stop-index and
   the string that matched."
  
  (bind ?string-index 1)
  (bind ?match (match ?pattern ?string))    ; search is implemented
                                            ; using match
  (while (and (eq ?match nil)
              (> (str-length ?string) 0))
    (bind ?string-index (+ ?string-index 1))
    (bind ?string (sub-string 2 (str-length ?string) ?string))
    (bind ?match (match ?pattern ?string)))
    
  (if (eq ?match nil)
   then (return (create$ -1 -1 nil))
   else (return (create$ ?string-index
                         (+ ?string-index (str-length ?match) -1)
                         ?match))))
                         
                         
(deffunction findall (?pattern ?string)
  "find and return information about all occurances of ?pattern in ?string.
   Returns a multifield of imploded "multifields" where each value consists
   of start-index, stop-index and the string that matched."
  
  (bind ?result (create$))
  (bind ?string-index 0)
  (while true
    (bind ?part (search ?pattern ?string))  ; findall is implemented
                                            ; using search
    (if (neq (nth$ 3 ?part) nil)
     then
       (bind ?string (sub-string (+ (nth$ 2 ?part) 1)    ; keep everything in
                                 (str-length ?string)    ; ?string after the
                                 ?string))               ; last match.
                                 
        ;; add ?string-index to begin- and end-index for this match.
        (bind ?part (replace$ ?part 1 1 (+ ?string-index (nth$ 1 ?part))))
        (bind ?part (replace$ ?part 2 2 (+ ?string-index (nth$ 2 ?part))))
        
        (bind ?string-index (nth$ 2 ?part))
        (bind ?result (insert$ ?result (+ (length$ ?result) 1) (implode$ ?part)))
        
     else (break)))
     
  (return ?result))
  
  
(deffunction split (?pattern ?string $?max)
  "split ?string in parts delimited by ?pattern. Empty strings ("") are
   removed from the result. If $?max is used the resulting multifield
   value will have at most $?max + 1 values."
   
  (bind ?string-index (nth$ 1 (search ?pattern ?string)))   ; split is
                                                            ; implemented
                                                            ; using search
  (bind ?string-anchor 1)
  (bind ?result (create$))
  
  (if (eq (length$ $?max) 0)
   then (bind ?max 0)
   else
     (if (> (length$ $?max) 1)
      then 
        (printout ?*errors* "; ERROR: Too many parameters." crlf)
        (printout ?*errors* "; The split function takes either 2 or 3 parameters.")
        (printout ?*errors* " You used " (+ (length$ $?max) 2) "!" crlf)
        (return nil)
      else (bind ?max (nth$ 1 $?max))))
      
  (while (neq ?string-index -1)
    (if (or (eq ?max 0)
            (> ?max (length$ ?result)))
     then
       (bind ?part (sub-string ?string-anchor
                               (- (+ ?string-index
                                     ?string-anchor) 2)
                               ?string))
       (bind ?result (insert$ ?result
                              (+ (length$ ?result) 1)
                              ?part))
       (bind ?string-anchor (+ ?string-index
                               (- ?string-anchor 1)
                               (str-length ?pattern)))
       (bind ?string-index (nth$ 1 (search ?pattern
                                           (sub-string ?string-anchor
                                                       (str-length ?string)
                                                       ?string))))
     else
       (bind ?part (sub-string ?string-anchor
                               (str-length ?string)
                               ?string))
       (bind ?result (insert$ ?result
                              (+ (length$ ?result) 1)
                              ?part))
       (return ?result)))
       
  (bind ?part (sub-string ?string-anchor
                          (str-length ?string)
                          ?string))
  (bind ?result (insert$ ?result
                         (+ (length$ ?result) 1)
                         ?part))
                         
  (bind ?fixed-result (create$))
  (progn$ (?part ?result)
     (if (or (neq ?part "")
             (eq ?part-index 1)
             (eq ?part-index (length$ ?result)))
      then (bind ?fixed-result (insert$ ?fixed-result
                               (+ (length$ ?fixed-result) 1)
                               ?part))))
  (return ?fixed-result))
  
  
(deffunction sub (?pattern ?replacement ?string $?max)
  "substitute each non-overlapping occurance of ?pattern in ?string with
   ?replacement."
   
  (bind ?result "")
  (if (eq (length$ $?max) 0)
   then (bind ?max 0)
   else
     (if (> (length$ $?max) 1)
      then 
        (printout ?*errors* "; ERROR: Too many parameters." crlf)
        (printout ?*errors* "; The sub function takes either 3 or 4 parameters.")
        (printout ?*errors* " You used " (+ (length$ $?max) 3) "!" crlf)
        (return nil)
      else (bind ?max (nth$ 1 $?max))))
      
  (bind ?string-parts (split ?pattern ?string ?max))    ; sub is implemented
                                                        ; using split
  (progn$ (?part ?string-parts)
    (bind ?result (str-cat ?result ?part))
    (if (< ?part-index (length$ ?string-parts))
     then (bind ?result (str-cat ?result ?replacement))))
     
  (return ?result))
  
  