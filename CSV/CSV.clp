;;;; A CLIPS utility for CSV-files (Comma Separated Values).
;;;;
;;;; These functions are meant to make it easier to import and export
;;;; facts from and to spreadsheet software such as Microsoft Excel.
;;;; The implementation follows the RFC 4180 specification (see
;;;; http://tools.ietf.org/html/rfc4180).
;;;;
;;;; 2008-03-31 v0.8	First version.
;;;; 2009-01-15 v0.9 	Fixed multislot and implied deftemplates in CSV-import.
;;;; 2009-01-15 v0.91 	Fixed string handling in multislots.
 
(defglobal ?*delimiter* = ",")
(defglobal ?*newline* = "
")
 
;;;; Helper functions
;;;; ----------------
 
(deffunction validate-template (?template $?slots)
  "Validate ?template together with $?slots.
 
   Prints an error message (to standard output) and returns FALSE if
   ?template is undefined (in the current module) or either of the names in
   $?slots aren't defined as slot-names in ?template."
 
  ;; Make sure that ?template is defined in current module
  (if (not (member$ ?template (get-deftemplate-list)))
   then
     (printout t "The deftemplate " ?template " does not exist in the current module.")
     (return FALSE))
 
  ;; Make sure that ?template contains each of the slots specified
  ;; in $?slots
  (if (> (length$ $?slots) 0)
   then
     (bind $?available-slots (deftemplate-slot-names ?template))
     (progn$ (?slot $?slots)
       (if (not (member$ (sym-cat ?slot) $?available-slots))
        then
          (printout t "The slot " ?slot " is not defined in " ?template crlf)
          (return FALSE))))
 
  (return TRUE))
 
(deffunction get-values (?line $?values)
  "Parse values from a string and return it as a multislot.
 
   $?values is the result of any previous calls to get-values. $?values
   always contains a token as the first item. The symbol OPEN is used to
   indicate that the first value in ?line is part of the last value in
   (rest$ $?values)."
 
  (bind ?token nil)
  (bind $?result (create$))
  (if (> (length$ $?values) 0)
   then
     (bind ?token (nth$ 1 $?values))
     (bind $?result (rest$ $?values)))
 
  (bind ?current-value nil)
  (bind $?temp (create$))
  (progn$ (?value $?result)
    (if (< ?value-index (length$ $?result))
     then (bind $?temp (create$ $?temp ?value))
     else (bind ?current-value ?value)))
  (bind $?result $?temp)
 
  (bind ?current-pos 1)
  (while (< ?current-pos (str-length ?line))
    ;; There are three states a value can be in: open and quoted,
    ;; closed and unquoted and closed and quoted.
    (if (eq ?token OPEN)
     then
       ;; The next value to read from ?line is part of
       ;; the last token in $?values.
 
       ;; Find the end-index of this value
       (bind ?end-index (str-index (str-cat "\"" ?*delimiter*) ?line))
 
       (if (eq ?end-index FALSE)
        then
          ;; Not found, this means either we're parsing
          ;; the last value on ?line or the value continues
          ;; on the next ?line. Try finding a closing "
          (bind ?end-index (str-index (str-cat "\"") ?line))
          (if (eq ?end-index (str-length ?line))
           then
             ;; The value ends at ?end-index
             (bind ?token CLOSED)
             (bind ?current-value (str-cat ?current-value
                                           ?*newline*
                                           (sub-string ?current-pos (- ?end-index 1) ?line)))
             (bind ?current-pos 1)
             (bind ?line (sub-string (+ ?end-index 2) (str-length ?line) ?line))
             (bind $?result (create$ $?result ?current-value))
           else
             ;; The value is all of ?line
             (bind ?token OPEN)
             (bind ?current-value (str-cat ?current-value ?line))
             (bind ?current-pos (str-length ?line))
             (bind $?result (create$ $?result ?current-value)))
        else
          ;; The value ends at ?end-index
          (bind ?token CLOSED)
          (bind ?current-value (str-cat ?current-value
                                        ?*newline*
                                        (sub-string ?current-pos ?end-index ?line)))
          (bind ?current-pos 1)
          (bind ?line (sub-string (+ ?end-index 2) (str-length ?line) ?line))
          (bind $?result (create$ $?result ?current-value)))
 
     else
       ;; Find the end-index of this value
       (bind ?quoted FALSE)
       (if (eq (sub-string ?current-pos ?current-pos ?line) "\"")
        then
          (bind ?token OPEN)
          (bind ?quoted TRUE)
          (bind ?end-index (str-index (str-cat "\"" ?*delimiter*) ?line))
        else
          (bind ?token CLOSED)
          (bind ?end-index (str-index (str-cat ?*delimiter*) ?line)))
 
       (if (eq ?end-index FALSE)
        then
          (bind ?current-value (sub-string ?current-pos (str-length ?line) ?line))
          (bind ?current-pos (str-length ?line))
          (bind $?result (create$ $?result ?current-value))
        else
          ;;
          (bind ?token CLOSED)
          (bind ?current-pos 1)
          (if (eq ?quoted TRUE)
           then
             (bind ?current-value (sub-string ?current-pos ?end-index ?line))
             (bind ?line (sub-string (+ ?end-index 2) (str-length ?line) ?line))
           else
             (bind ?current-value (sub-string ?current-pos (- ?end-index 1) ?line))
             (bind ?line (sub-string (+ ?end-index 1) (str-length ?line) ?line)))
          (bind $?result (create$ $?result ?current-value)))))
 
    (return (create$ ?token $?result)))
 
(deffunction encode-string-delimiters (?string)
  "Encode all string delimiters in ?string.

   Each \" in ?string becomes \"\"."

  (bind ?result "")
  (loop-for-count (?c 0 (length$ ?string))
    (bind ?curr-position (- (length$ ?string) ?c))
    (bind ?char (sub-string ?curr-position ?curr-position ?string))
    (if (eq ?char "\"")
     then (bind ?result (str-cat "\"" ?result)))
    (bind ?result (str-cat ?char ?result)))
  (return ?result))
 
(deffunction decode-string-delimiters (?string)
  "Decode all string delimiters in ?string.

   Each \"\" in ?string becomes \". NOTE! Calling this function with
   a string that has not been encoded first will most probably not
   result in anything useful."

  (bind ?result "")
  (bind ?skip FALSE)
  (loop-for-count (?c 0 (- (length$ ?string) 1))
    (bind ?curr-position (- (length$ ?string) ?c))
    (bind ?seq (sub-string ?curr-position (+ ?curr-position 1) ?string))
    (if (eq ?seq "\"\"")
     then (bind ?skip TRUE))
    (if (not ?skip)
     then (bind ?result (str-cat (sub-string ?curr-position ?curr-position ?string) ?result))
     else (bind ?skip FALSE)))
  (return ?result))
 
;;;; Public functions
;;;; ----------------
 
(deffunction CSV-export (?filename ?template $?slots)
  "Export facts to ?filename in CSV format.
 
   $?slots specify which slots should be exported. If no $?slots are
   specified, all slots in ?template are exported."
 
  (if (not (validate-template ?template $?slots))
   then (return FALSE))
 
  (if (eq (length$ $?slots) 0)
   then (bind $?slots (deftemplate-slot-names ?template)))
 
  (open ?filename CSV-OUTPUT "w")
 
  ;; Write headers to file.
  (progn$ (?slot $?slots)
    (printout CSV-OUTPUT ?slot)
    (if (< ?slot-index (length$ $?slots))
     then (printout CSV-OUTPUT ?*delimiter*)))
  (printout CSV-OUTPUT crlf)
 
  ;; Write facts to file
  (bind ?count 0)
  (do-for-all-facts ((?fact ?template)) TRUE
    (progn$ (?slot $?slots)
      (bind ?slot-value (fact-slot-value ?fact ?slot))
      (if (or (eq (type ?slot-value) STRING)                ; Strings and
              (deftemplate-slot-multip ?template ?slot))    ; multislots are
       then   			       		 	    ; quoted.
         (if (deftemplate-slot-multip ?template ?slot)
          then (printout CSV-OUTPUT "\"" (encode-string-delimiters (implode$ ?slot-value)) "\"")
	  else (printout CSV-OUTPUT "\"" ?slot-value "\""))
       else (printout CSV-OUTPUT ?slot-value))
 
      (if (< ?slot-index (length$ $?slots))
       then (printout CSV-OUTPUT ?*delimiter*)))
 
    (printout CSV-OUTPUT crlf)
    (bind ?count (+ ?count 1)))
 
  (close CSV-OUTPUT)
  (return ?count))
 
 
(deffunction CSV-import (?filename ?template $?slots)
  "Import facts from ?filename.
 
   Each row in ?filename will be asserted as a fact (using the deftemplate
   ?template). Each column (comma separated value) in the row is used as
   the corresponding slot-value as specified in $?slots. If no slot-names
   are provided, the first row in ?filename must contain slot-names."
 
  (if (not (validate-template ?template $?slots))
   then (return FALSE))
 
  (bind ?count 0)
  (open ?filename CSV-INPUT)
 
  ;; If no $?slots are provided we'll use the first
  ;; row in ?filename instead which means we'll also
  ;; have to validate the ?template and $?slots again.
  (if (eq (length$ $?slots) 0)
   then
     (bind ?line (readline CSV-INPUT))
     (if (eq ?line EOF)
      then
        (close CSV-INPUT)
        (return ?count))
 
     (bind $?slots (rest$ (get-values ?line)))
     (if (not (validate-template ?template $?slots))
      then
        (close CSV-INPUT)
        (return FALSE)))
 
  (bind $?values (create$))
  (bind ?done FALSE)
  (while (not ?done)
    (bind ?done (eq (bind ?line (readline CSV-INPUT)) EOF))
    (if (not ?done)
     then
       (if (> (length$ $?values) (length$ $?slots))
        then ;; Return, this file is corrupt.
          (close CSV-INPUT)
          (return ?count))
 
        (bind $?values (get-values ?line $?values))
        (if (eq (length$ (rest$ $?values)) (length$ $?slots))
         then
           (if (and (eq (length$ $?slots) 1)
                    (eq (nth$ 1 $?slots) "implied"))
            then ; Handle implied deftemplates
              (bind ?value (nth$ 1 (rest$ $?values)))
              (assert-string (str-cat "(" ?template " "
	      		     	      (decode-string-delimiters (sub-string 2 (- (str-length ?value) 1) ?value))
				      ")"))
            else
              (bind ?fact (str-cat "(" ?template))
              (progn$ (?value (rest$ $?values))
                (if (deftemplate-slot-multip ?template (sym-cat (nth$ ?value-index $?slots)))
                 then ; Multislots
                   (bind ?fact (str-cat ?fact " (" (nth$ ?value-index $?slots) " "
		   	       		(decode-string-delimiters (sub-string 2 (- (str-length ?value) 1) ?value))
					")"))
                 else ; Slots
                   (bind ?fact (str-cat ?fact " (" (nth$ ?value-index $?slots) " " ?value ")"))))
              (assert-string (str-cat ?fact ")")))
 
           (bind $?values (create$))
           (bind ?count (+ ?count 1)))))
 
  (close CSV-INPUT)
  (return ?count))