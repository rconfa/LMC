;;;; -*- Mode: Common Lisp -*-
;;;; lmc.lisp
;;;; A simulator for the Little Man Computer.



;;; Defines a struct to save all the lines read from the file. 
;;; All lines must match with one of the following case:
;;; 1) INSTRUCTION
;;; 2) INSTRUCTION XX 
;;; 3) INSTRUCTION LABEL 
;;; 4) LABEL INSTRUCTION 
;;; 5) LABEL INSTRUCTION XX
;;; 6) LABEL INSTRUCTION LABEL
;;; label-name: has a value only in the 4-5-6 case.
;;; inst-name: will always have a value.
;;; value-name: will always have a value except in case 1. The value could 
;;;            be a number or the name of a defined label.

(defstruct instr 
  label-name 
  inst-name 
  value-name)


;;; lmc-run takes a file path and a list that represents the memory content 
;;; of input values as argument.
;;; Loads the file (filename) and creates the corresponding numerical list
;;; (mem-file).
;;; Then executes every instruction starting from 0, with the 
;;; input list given (list-input) and produces the corresponding output (Out).

(defun lmc-run (filename list-input)
  (let* ((mem-file (lmc-load Filename))
	 (out (execution-loop (list 'state 
				    :acc 0 
				    :pc 0 
				    :mem mem-file 
				    :in list-input
				    :out '()
				    :flag 'noflag))))
    (format t "~%The Output list is: ")
    out))


;;; lmc-load takes a file path as argument.
;;; Reads the file (Filename) and saves the result in a structure (s-instr),
;;; then checks if the length of the file readed is valid.
;;; After saves all the labels (list-label) and checks that there aren't
;;; duplicate labels. The list-label contains all the defined label names and 
;;; their position like [NameLabel1, 0, NameLabel2, 9] where 0 and 9 are the 
;;; positions in which the labels are defined.
;;; Next converts all instructions into the corresponding numerical code 	
;;; (mem-to-fill) and checks that the list is fill correctly (100 elements).
;;; If all these controls are true, the function generates the final list.
;;; If the file contains more than 100 instruction the function fails 
;;; with an error message (also the whole program fails).

(defun lmc-load (filename)
  (let ((s-instr (my-read-file filename))) 
    (if (<= (length s-instr) 100) 
        (let ((list-label (save-labels s-instr)))
          (if (no-dupl list-label)                    
              (let ((mem-to-fill (convert s-instr list-label)))
                (check-if-fill mem-to-fill))
            (error "File too large."))))))


;;; my-read-file takes a file path as argument.
;;; Opens the file, cleans every line read, if it has not read an empty line 
;;; (or just spaces) it saves the relative structure obtained from the read 
;;; line and recursively continues to read.
;;; Instead if it reads an empty line, it doesn't save it and continues 	
;;; reading.

(defun my-read-file (filename)
  (with-open-file (f filename :direction :input :if-does-not-exist :error)
		  (labels ((read-helper ()
					(let ((line (read-line f nil nil)))
					  (when line 
					    (let ((correct-line (clean-line line)))
					      (if (String/= "" correct-line) 
						  (cons (save-in-struct correct-line) (read-helper)) 
						(read-helper))))))) 
			  (read-helper))))


;;; clean-line takes a string as argument.
;;; Cleans up the string, removes initials and finals spaces and converts the 
;;; string to uppercase.

(defun clean-line (str)
  (string-upcase (string-trim '(#\Space #\Newline #\Backspace #\Tab 
				#\Linefeed #\Page #\Return #\Rubout)
			      (clean-comment str))))


;;; clean-comment takes a string as argument.
;;; Looks for the position of the first "/" that indicates the beginning of a 
;;; possible comment, if it doesn't exist it leaves the string unchanged.
;;; Instead if there is the first "/", checks if in the next position
;;; there is the other "/", if yes it takes the whole string starting from 	
;;; position 0 up to the beginning of the comment.
;;; Otherwise, if the second "/" does not exist, the function fails with 
;;; an error message (also the whole program fails).

(defun clean-comment (str)
  (let ((slash-pos (position #\/ str)))
    (if slash-pos 
        (let ((second-slash (position #\/ str :start (1+ slash-pos))))
	  (if (and second-slash (= second-slash (1+ slash-pos)))
              (subseq str 0  slash-pos)
            (error "~S contains a not valid comment" str)))
      str)))


;;; my-split takes a string as argument.
;;; Saves the position of the first space in the string (ind-space).
;;; If the substring from 0 up to ind-space returns something (it is not a 	
;;; single space character) it saves the result and continues to split the 	
;;; string.
;;; Otherwise if the substring is empty, it has taken a space and it doesn't 
;;; save it, then it will continue to split the string.
;;; In this way it deletes all spaces in the string.
;;; If the function doesn't find a space the string must be a single word
;;; (or the last word of a more word string) and saves the last word.

(defun my-split (str)
  (let ((ind-space (position #\Space str)))
    (if ind-space
	(let ((substr (subseq str 0 ind-space)))
	  (if (string/= "" substr)
	      (cons substr (my-split (subseq str (1+ ind-space) 
                                             (length str)))) 
            (my-split (subseq str (1+ ind-space) (length str)))))
      (cons str nil))))


;;; check-if-fill takes a list as argument.
;;; Checks if the length of the list (len) is 100.
;;; If it's less than 100 it fill the list by appending a new list with all 0.

(defun check-if-fill (lst)
  (let ((len (length lst)))
    (if (< len 100) 
        (append lst (make-list (- 100 len) :initial-element 0))
      lst)))


;;; save-in-struct takes a string as argument.
;;; Saves in a structure all the elements read from the string by split it and
;;; checking the syntax of each element.
;;; The splitted string saved like a list must be must be at most of 3 words 
;;; (length of the list <= 3).
;;; If the length is 1 is the case of single istruction like inp/out/hlt/dat.
;;; If the length is 2 it could be an istruction with value like "add xx" or 
;;; a define of a valid label followed by a single instruction like "label inp"
;;; If the length is 3 it must be the define of a valid label followed by an 
;;; instruction with value like "label add xx".
;;; Notice that xx in case 2 and 3 could be a number or a valid label name
;;; If the string don't match with one of this case the function fails 
;;; with an error message (also the whole program fails).

(defun save-in-struct (str) 
  (let* ((lst (my-split str)) 
    	 (len (length lst)) 
         (1st (car lst))
         (2nd (cadr lst)) 
         (3th (caddr lst))) 
    (cond  
     ((and (= 1 len) (is-single-inst 1st))
      (make-instr :inst-name (read-from-string 1st)))
     ((and (= 2 len) (is-inst-with-value 1st)
           (not (valid-inst 2nd))) 
      (make-instr :inst-name (read-from-string 1st)
                  :value-name (read-from-string 2nd)))
     ((and (= 2 len) (valid-label 1st) (is-single-inst 2nd))
      (make-instr :label-name (read-from-string 1st)
                  :inst-name (read-from-string 2nd)))
     ((and (= 3 len) (valid-label 1st)
           (is-inst-with-value 2nd) (not (valid-inst 3th)))
      (make-instr :label-name (read-from-string 1st)
		  :inst-name (read-from-string 2nd)
		  :value-name (read-from-string 3th)))
     (t (error "~S invalid instruction." str)))))


;;; valid-inst takes a string as argument.
;;; Checks if is a valid name for an instruction.

(defun valid-inst (str)
  (when (or (is-inst-with-value str) (is-single-inst str)) 
    t))


;;; is-inst-with-value takes a string as argument.
;;; Checks if is a valid name for an instruction with value.

(defun is-inst-with-value (str)
  (when (or (String= str "ADD") (String= str "SUB") (String= str "STA")
	    (String= str "LDA") (String= str "BRA") (String= str "BRZ")
	    (String= str "BRP") (String= str "DAT")) 
    t))


;;; is-single-inst takes a string as argument.
;;; Checks if is a valid name for a single instruction.

(defun is-single-inst (str)
  (when (or (string= str "INP") (string= str "OUT") 
            (string= str "HLT")	(string= str "DAT")) 
    t))



;;; save-labels takes a list of the structure instr and an optional 
;;; starting index as arguments. The starting index is needed for append the 
;;; correct line number (ind) after the name of the label in the list.
;;; For each readed line from file checks if is the define of a label.
;;; If exists the name of a label (lab-name) in the first element of the list
;;; saves it in a list and append the index (ind) of the line where it has 	
;;; found it, then continues to check the rest of the list.
;;; Otherwise if there isn't the lab-name, it skips the first element of the 
;;; list and continues to check the rest of the list.

(defun save-labels (lst &optional (ind 0))
  (when lst
    (let* ((elem (car lst))
           (lab-name (instr-label-name elem)))
      (cond 
       (lab-name
        (cons lab-name 
              (cons ind (save-labels (cdr lst) (1+ ind)))))
       (t (save-labels (cdr lst) (1+ ind)))))))


;;; no-dupl takes a list as argument.
;;; Checks if the list given by argument has a duplicate values.
;;; For each control skip two elments because the list is like
;;; [NameLabel Index NameLabel2 Index2]
;;; But the index will never match with another value in the list so it could
;;; be ignored.
;;; If the function found a duplicate values it will fails with an error 
;;; message (also the whole program fails).

(defun no-dupl (lst)
  (cond 
   ((null lst) t)
   ((member (car lst) (cddr lst)) 
    (error "~S, this label name is duplicate!" (car lst)))
   (t (no-dupl (cddr lst)))))


;;; convert takes the structure created before and the list of all label for 
;;; arguments.
;;; Converts all the instructions present in the structure in numerical values
;;; by calling the function create-op-code with the right parameteres.
;;; If the structure is empty this function terminates without do anything.
;;; The label-name in never taken because it was already save in the list label
;;; and so it's useless in this function.
;;; All the inst-name presents in the given structure are correct, so this 
;;; function must checks only if the value is valid.
;;; The value as before could be a number or a defined label present in the 
;;; list label.

(defun convert (str-instr list-label)
  (when str-instr
    (let ((inst (instr-inst-name (car str-instr)))
          (val (instr-value-name (car str-instr))))
      (cond 
       ;; if is an istruction with value (no a label name) checks that 
       ;; the value is in the range [0,99]. Dat is checked later 
       ;; because could have or not a value and however it's correct
       ;; if something is wrong the function fails 
       ;; with an error message (also the whole program fails).
       ((and (not (eq inst 'DAT)) (numberp val))
        (if (and (>= val 0) (< val 100))
            (cons (create-op-code inst val) 
                  (convert (cdr str-instr) list-label))
          (error "~S ~D, invalid value" inst val)))
       ;; if is a DAT instruction it checks if as a value, if yes the 
       ;; value must be in the range [0, 999] otherwise is like
       ;; the value is 0.
       ;; if something is wrong the function fails 
       ;; with an error message (also the whole program fails).
       ((if (and (eq inst 'DAT) (numberp val) (>= val 0) (< val 1000))
            (cons (create-op-code inst val) 
                  (convert (cdr str-instr) list-label))
          (when (and (eq inst 'DAT) (numberp val))
            (error "~S ~D, invalid value" inst val))))
       ;; if is a single instruction it checks that there isn't a 
       ;; value, if something is wrong the function fails 
       ;; with an error message (also the whole program fails).
       ((is-single-inst inst)
        (if (eq nil val)
            (cons (create-op-code inst nil) 
                  (convert (cdr str-instr) list-label))
          (error "~S ~D, invalid instrution" inst val)))
       ;; if never match with the case before it must be an instruction
       ;; with a label value so it take the position of the defined 
       ;; label from the list-label. If there isn't the label in the 
       ;; list-label it will fail.
       ;; if something is wrong the function fails 
       ;; with an error message (also the whole program fails).
       ((if (member val list-label)
            (cons 
             (create-op-code inst 
                             (nth (1+ (position val list-label)) list-label)) 
             (convert (cdr str-instr) list-label))
          (error "~S ~S, this label name is not define!" inst val)))
       ;; If all the case never match it's a wrong instruction
       ;; the function fails with an error message 
       ;; (also the whole program fails).
       (t (error "~S ~D, invalid instruction" inst val))))))


;;; create-op-code takes the name of an instruction and is eventually value 
;;; as arguments.
;;; This function creates the op-code of the instruction (inst) based on the 
;;; name inst and his value.

(defun create-op-code (inst val)
  (cond 
   ((eq inst 'ADD) (+ 100 val)) 
   ((eq inst 'SUB) (+ 200 val))
   ((eq inst 'STA) (+ 300 val))
   ((eq inst 'LDA) (+ 500 val))
   ((eq inst 'BRA) (+ 600 val))
   ((eq inst 'BRZ) (+ 700 val))
   ((eq inst 'BRP) (+ 800 val))
   ((eq inst 'INP) 901)
   ((eq inst 'OUT) 902)
   ((eq inst 'HLT) 0)
   ((if (and (eq inst 'DAT) val)
        val
      0))))


;;; valid-label takes a string as argument.
;;; Checks if the given string contains only char number or _.
;;; If the string contains other characters the function fails 
;;; with an error message (also the whole program fails).

(defun valid-label (str)
  (if (or (alpha-char-p (char str 0)) (string= #\_ (subseq str 0 1))) 
      (if (every 'valid-char (subseq str 1))
          str
        (error "~S, invalid label name" str))
    (error "~S, label name must start with char or _" str)))


;;; valid-char takes a char as argument.
;;; Checks if the given char is the underscore or an alphanumeric char.

(defun valid-char (c)
  (when (or (eq #\_ c) (alphanumericp c)) t))


;;; execution-loop takes a list, that represents the LMC state, as argument.
;;; If the memory length is not 100, the function fails with an error
;;; message (also the whole program fails).
;;; If the State is a state (and not an halted state), the program execution
;;; continues with the next instruction.
;;; If the State is an halted state, returns the output list.

(defun execution-loop (State) 
  (if (= (length (nth 6 State)) 100)
      (let* ((new-state (one-instruction State))
	     (type-state (nth 0 new-state)))
	(if (eql type-state 'State)
	    (execution-loop new-state)
	  (nth 10 new-state)))
    (error "Invalid memory length")))


;;; one-instruction takes a list, that represents the LMC state, as argument.
;;; Reads the current instruction from the memory and executes it by calling 
;;; the correct function.
;;; Returns a list that that represents the new LMC state (or halted state).

(defun one-instruction (State) 
  (let* ((acc (nth 2 State))
	 (pc (nth 4 State))
	 (mem (nth 6 State))
	 (in (nth 8 State))
	 (out (nth 10 State))
	 (flag (nth 12 State))
	 (istr (nth pc mem)))
		  
    (multiple-value-bind (first-digit cell-number) (floor istr 100)
			 (cond ((= first-digit 0) (halt-operation acc pc mem in out flag))
			       ((= first-digit 1) (add-operation cell-number acc pc mem in 
								 out))
			       ((= first-digit 2) (sub-operation cell-number acc pc mem in
                   						 out))
			       ((= first-digit 3) (store-operation cell-number acc pc mem
				  				   in out flag))
			       ((= first-digit 5) (load-operation cell-number pc mem in out
                   						  flag))
			       ((= first-digit 6) (branch-operation cell-number acc mem in 
								    out flag))
			       ((= first-digit 7) (branch-if-zero-operation cell-number acc
                   							    pc mem in out flag))
			       ((= first-digit 8) (branch-if-positive-operation cell-number 
                  								acc pc mem in out flag))
			       ((and (= first-digit 9) (= cell-number 1)) (input-operation  
									   pc mem in out flag))
			       ((and (= first-digit 9) (= cell-number 2)) (output-operation 
									   acc pc mem in out flag))
			       (T (illegal-instruction-operation))))))


;;; halt-operation takes an integer that represents the accumulator value, an 
;;; integer that represents the program counter value, a list that represents 
;;; the memory content, a list that represents the input list content, a list 
;;; that represents the output list content and a quoted word that represents 
;;; the flag value as arguments.
;;; If the first digit of the instruction is a 0, the instruction is an halt 
;;; operation.
;;; Returns a list that represents the LMC halted state.

(defun halt-operation (acc pc mem in out flag) 
  (list 'halted-state
	:acc acc
	:pc pc
	:mem mem 
	:in in 
	:out out 
	:flag flag))
	  

;;; add-operation takes an integer that represents the cell number value, an 
;;; integer that represents the accumulator value, an integer that represents 
;;; the program counter value, a list that represents the memory content, a 
;;; list that represents the input list content and a list that represents the 
;;; output list content as arguments.
;;; If the first digit of the instruction is 1, the instruction is an add 	
;;; operation.
;;; Sums the accumulator value (acc) with the memory cell content at 		
;;; cell-number position and writes the result (mod 1000) in the accumulator 
;;; (acc). Updates the program counter (pc) and returns a list that represents 
;;; the new LMC state.
;;; If the result is more than 0 and less than 1000, the flag value (flag) is 
;;; "noflag"; otherwise the flag value (flag) is "flag".

(defun add-operation (cell-number acc pc mem in out)
  (multiple-value-bind (new-acc new-flag) (update-acc acc 
  						      (nth cell-number mem))
		       (list 'state
			     :acc new-acc
			     :pc (update-pc pc)
			     :mem mem 
			     :in in 
			     :out out 
			     :flag new-flag)))
		  

;;; sub-operation takes an integer that represents the cell number value, an 
;;; integer that represents the accumulator value, an integer that represents 
;;; the program counter value, a list that represents the memory content, a 
;;; list that represents the input list content and a list that represents the 
;;; output list content as arguments.
;;; If the first digit of the instruction is 2, the instruction is a sub 	
;;; operation.
;;; Subtracts the memory cell content at cell-number position from the 		
;;; accumulator value (acc) and writes the result (mod 1000) in the 		
;;; accumulator (acc). Updates the program counter (pc) and returns a list 	
;;; that represents the new LMC state.
;;; If the result is more than 0 and less than 1000, the flag value (flag) is 
;;; "noflag"; otherwise the flag value (flag) is "flag".

(defun sub-operation (cell-number acc pc mem in out) 
  (multiple-value-bind (new-acc new-flag) 
		       (update-acc acc (- 0 (nth cell-number mem)))
		       (list 'state
			     :acc new-acc
			     :pc (update-pc pc)
			     :mem mem 
			     :in in 
			     :out out 
			     :flag new-flag)))


;;; store-operation takes an integer that represents the cell number value, an 
;;; integer that represents the accumulator value, an integer that represents 
;;; the program counter value, a list that represents the memory content, a 
;;; list that represents the input list content, a list that represents the 
;;; output list content and a quoted word that represents the flag value for 		
;;; arguments.
;;; If the first digit of the instruction is 3, the instruction is a store 	
;;; operation.
;;; Saves the accumulator value (acc) in the memory cell at cell-number 	
;;; position. The accumulator content (acc) remains unchanged. Updates the 	
;;; program counter (pc) and returns a list that represents the new LMC state.

(defun store-operation (cell-number acc pc mem in out flag) 
  (setf (nth cell-number mem) acc)
  (list 'state
	:acc acc
	:pc (update-pc pc)
	:mem mem
	:in in 
	:out out 
	:flag flag))


;;; load-operation takes an integer that represents the cell number value, an 
;;; integer that represents the program counter value, a list that represents 
;;; the memory content, a list that represents the input list content, a list 
;;; that represents the output list content and a quoted word that represents 
;;; the flag value as arguments.
;;; If the first digit of the instruction is 5, the instruction is a load 	
;;; operation.
;;; Writes the memory cell content at cell-number position in the accumulator 
;;; (acc). The accumulator content (acc) remains unchanged. Updates the 		
;;; program counter (pc) and returns a list that represents the new LMC state.

(defun load-operation (cell-number pc mem in out flag) 
  (list 'state
	:acc (nth cell-number mem)
	:pc (update-pc pc)
	:mem mem
	:in in 
	:out out 
	:flag flag))


;;; branch-operation takes an integer that represents the cell number value, 
;;; an integer that represents the accumulator value, a list that represents 
;;; the memory content, a list that represents the input list content, a list 
;;; that represents the output list content and a quoted word that represents 
;;; the flag value as arguments.
;;; If the first digit of the instruction is 6, the instruction is a branch 
;;; operation.
;;; Unconditional jump: sets the program counter value (pc) to cell-number. 
;;; Returns a list that represents the new LMC state.

(defun branch-operation (cell-number acc mem in out flag) 
  (list 'state
	:acc acc
	:pc cell-number
	:mem mem
	:in in 
	:out out 
	:flag flag))


;;; branch-if-zero-operation takes an integer that represents the cell number 
;;; value, an integer that represents the accumulator value, an integer that 
;;; represents the program counter value, a list that represents the memory 
;;; content, a list that represents the input list content, a list that 		
;;; represents the output list content and a quoted word that represents the 
;;; flag value as arguments.
;;; If the first digit of the instruction is 7, the instruction is a branch if 
;;; zero operation.
;;; Conditional jump: if the accumulator content (acc) is 0 and the flag value 
;;; (flag) is "noflag", sets the program counter value (pc) to cell-number. 
;;; Otherwise, if the accumulator content (acc) is not 0 or the flag value 	
;;; (flag) is "flag", updates the program counter value (pc) and returns a 	
;;; list that represents the new LMC state.

(defun branch-if-zero-operation (cell-number acc pc mem in out flag) 
  (list 'state
	:acc acc
	:pc (if (and (= acc 0) (eql flag 'noflag))
		cell-number
	      (update-pc pc))
	:mem mem
	:in in 
	:out out 
	:flag flag))


;;; branch-if-positive-operation takes an integer that represents the cell 	
;;; number value, an integer that represents the accumulator value, an  	
;;; integer that represents the program counter value, a list that represents  
;;; the memory content, a list that represents the input list content, a list  
;;; that represents the output list content and a quoted that represents the 
;;; flag value as arguments.
;;; If the first digit of the instruction is 8, the instruction is a branch if 
;;; flag positive operation.
;;; Conditional jump: if the flag value (flag) is "noflag", sets the program 
;;; counter value (pc) to cell-number. Otherwise, if the flag value (flag) is 
;;; "flag", updates the program counter value (pc) and returns a list that 
;;; represents the new LMC state.

(defun branch-if-positive-operation (cell-number acc pc mem in out flag) 
  (list 'state
	:acc acc
	:pc (if (eql flag 'noflag)
		cell-number
	      (update-pc pc))
	:mem mem
	:in in 
	:out out 
	:flag flag))


;;; input-operation takes an integer that represents the program counter 	
;;; value, a list that represents the memory content, a list that represents 
;;; the input list content, a list that represents the output list content and 
;;; a quoted word that represents the flag value as arguments.
;;; If the instruction number is 901, the instruction is an input operation.
;;; If the input list (in) is not empty, writes the first element of the input 
;;; list in the accumulator value (acc), removing it from the list.
;;; Updates the program counter (pc) and returns a list that represents the 
;;; new LMC state.
;;; If the input list is empty, the function fails with an error message 	
;;; (also the whole program fails).

(defun input-operation (pc mem in out flag) 
  (list 'state
	:acc (let ((first-elem (car in)))
               (cond ((null in) (error "Input list is empty"))
                     ((or (< first-elem 0) (> first-elem 999)) 
                      (error "Invalid input list"))
                     (t first-elem)))
	:pc (update-pc pc)
	:mem mem
	:in (cdr in) 
	:out out 
	:flag flag))


;;; output-operation takes an integer that represents the accumulator value, 
;;; an integer that represents the program counter value, a list that 			
;;; represents the memory content, a list that represents the input list 	
;;; content, a list that represents the output list content and a quoted word 
;;; that represents the flag value as arguments.
;;; If the instruction number is 902, the instruction is an output operation.
;;; Writes the accumulator value (acc) at the end of the output list (out). The
;;; accumulator content remains unchanged. Updates the program counter (pc) 
;;; and returns a list that represents the new LMC state.

(defun output-operation (acc pc mem in out flag) 
  (list 'state
	:acc acc
	:pc (update-pc pc)
	:mem mem
	:in in 
    :out (cons-end acc out)
	:flag flag))


;;; illegal-instruction-operation takes no values as arguments.
;;; If all the cond conditions in the one-instruction function fail, the 	
;;; instruction is an illegal instruction operation.
;;; Ranges of illegal-instruction-operation: [400-499], 900, [903-999].
;;; The function fails with an error message (also the whole program fails).

(defun illegal-instruction-operation ()
  (error "Illegal instruction"))


;;; cons-end takes an integer and a list as arguments.
;;; Inserts an item (elem) at the end of the list (list). If the list is 	
;;; empty, returns a list that contains only the element. Otherwise, inserts 
;;; the first element of the list (car list) at the start of the list created 
;;; by calling recursively the cons-end function with the rest of the list 	
;;; (cdr list).

(defun cons-end (elem list) 
  (cond ((null list) (list elem))
	((atom list) (error "The second argument must be a list!"))
	(t (cons (car list) (cons-end elem (cdr list))))))


;;; update-Acc takes an integer that represents the accumulator value and an
;;; integer that represents the cell content as arguments.
;;; Sums the accumulator value (acc) with the memory cell content 			
;;; (cell-content). Returns two values (which are evaluated) that will be used 
;;; by the caller. The first is the result (mod 1000) of the sum and the 	
;;; second is a string that represents the flag value (it is "noflag" if the 
;;; result of the sum is less than 999 and more than 0; otherwise it is 		
;;; "flag").  

(defun update-acc (acc cell-content)
  (let ((sum (+ acc cell-content)))
    (values (mod sum 1000)
	    (if (or (> sum 999) (< sum 0)) 
		'flag 
	      'noflag))))


;;; update-pc takes an integer that represents the program counter value for
;;; argument.
;;; Updates the program counter (pc) by increasing it by 1 (mod 100).

(defun update-pc (pc)
  (mod (+ 1 pc) 100))


;;;; end of file -- lmc.lisp
