(in-package :nxt-disassem)


;;--------------------------------------------------------------------------------
;; LEGO NXT specific types
;;--------------------------------------------------------------------------------
	     
(defmethod print-object ((o dstoc-entry) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S ~S ~S" (dstoc-type o)
	    (flags o)
	    (data-descriptor o))))
	    
  

(defun print-instr (instr labels)
  "Returns the string presentation of a single instruction `instr'.
The `labels' are a hashtable mapping addresses to label names.  
This is used to give a symbolic name to offsets present in the instruction."
  (let* ((arguments (cdddr instr))
	 (offset (getf arguments :offset)))
    (format nil "     ~15A ~{ ~A~}"
	    (getf (rest instr) :instruction)
	    (if offset
		(append (list :target (gethash (+ (car instr) offset) labels)) 
			arguments)
		arguments))))

	
(defmethod print-object ((o clump-record-fixed) stream) 
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "FC: ~S, DC: ~S, CSO: ~S"
	    (fire-count o)
	    (dependency-count o)
	    (code-start-offset o))))

    
(defun parse-nxt-rxe-file (file-name)
  "Reads a NXT rxe file named by `file-name' and parses it.
The return value is an instance of nxt-rxe-file."
  (with-open-file (s file-name :element-type '(unsigned-byte 8))
    (read-value 'nxt-rxe-file s)))

;(defun test-parse ()  (parse-nxt-rxe-file "/Users/woudshoo/Development/nxt/ttt.rxe"))
;(defun test-parse-2 () (parse-nxt-rxe-file "/Users/woudshoo/Development/Source/Lego/test.rxe"))
;(defun test-parse-3 () (parse-nxt-rxe-file "/Users/woudshoo/Development/Source/Lego/test2.rxe"))
;(defun test-parse-4 () (parse-nxt-rxe-file "/Users/woudshoo/Development/Source/Lego/test3.rxe"))

(defun clump-starts-at (fixed-clump-records address)
  "Function that will check if address is the start of a clump.
Returns nil if the address is not the start of a clump, 
or returns the clump index if it is the start of a clump."
  (position address fixed-clump-records :test #'eql :key #'code-start-offset))

(defun print-data (p)
  "Print data section in somewhat user friendly format."
  (print-entries (test-create-full p)))

(defun print-entries (p &optional (stream t) (indent 0))
  "Prints a list of data entries to `stream'.
This function is very ugly and needs to be restructured.
One day I should understand how the pretty printer works."
  (loop 
     :with indent-string = (coerce (make-array indent :initial-element #\Space) 'string)
     :for entry :in p
     :for index :from 1
     :for type = (type (car entry))
     :for value = (cdr entry)
     :do
     (case type
       (:tc-cluster (format stream "~A~A V~D = {~%" indent-string
			    type index)
		    (print-entries value stream (+ indent 4))
		    (format stream "~A}~%" indent-string))
       (:tc-array (format stream "~A~A V~D = (" indent-string type index)
		  (if value
		      (progn
			(format stream "~%")
			(print-entries value stream (+ indent 4))
			(format stream "~A)~%" indent-string))
		      (format stream ")~%")))
       (:tc-ubyte 	
	(format stream "~A~A V~D = ~A ; ~A~%" indent-string type index value
		(code-char value)))
       (otherwise
	(format stream "~A~A V~D = ~A~%" indent-string type index value)))))

(defun print-assembly (p)
  "Prints a user readable version of the assembly code
contained in the program `p'.  The argument `p' should
be an instance of nxt-rxe-file class, as can be obtained
by parse-nxt-rxe-file."
  (loop 
     :with parsed-code = (parse-code (code-words (code-words p)))
     :with labels = (label-targets (get-targets parsed-code))
     :for instr :in parsed-code
     :for address = (car instr)
     :for opcode = (cdr instr)
     :for clump-start = (clump-starts-at (fixed-clump-data (clump-records p))
					 address)
     :do
     (when clump-start
       (format t "~&~%START OF CLUMP: ~D" clump-start))
     (when (gethash address labels)
       (format t "~&~A:" (gethash address labels)))
#+nil     (format t "~&~A" (opcode-data-to-string instr))
     (format t "~&~A" (print-instr instr labels)))
  (format t "~&"))


#|
(defun print-data (name data dstoc-entry dope-vector dynamic-data dynamic-data-offset)
  "Prints a single instruction"
  (case (dstoc-type dstoc-entry)
    (:tc-cluster 
     (format t "(~A~%" name)
     (loop :for sub-entry :in (rest dstoc-entry)
	:for sub-data :in data
	:for index :from 1
	:for sub-name = (format nil "~A.~A" name index)
	:do 
	(print-data sub-name sub-data sub-entry dope-vector dynamic-data dynamic-data-offset ))
     (format t ")~%"))
    
    (:tc-array)
    ( t 
     (format t "(~A ~A ~A)~%" name (dstoc-type dstoc-entry) data))))

(defun print-initial-data (p)
  "Prints a user understandable version of the initial variables with initial values.
The printed format is:

 (short V1 10)
 (cluster V2 
   (short V2.1 0)
   (array V2.2 #((short V2.2.1 10) (short V2.2.2 100))))
 "
  (let ((dd (default-dynamic-data p))
	(do (static-size (data-space-header p)))
	(dstoc-tree (parse-dstoc-table (dstoc-table p)))
	(data (default-static-data p)))
    (loop 
       :for dstoc-entry :in dstoc-tree
       :for data-entry :in data
       :for index :from 1
       :for name = (format nil "V~A" index)
       :do
       (print-data name data-entry dstoc-entry nil dd do))))

|#

(defun make-call-graph (p &optional (s t))
  "Writes the call graph of program `p' in the graphviz DOT file format to the stream `s'.
`p' should be an instance of the nxt-rxe-file class as can be obtained by parse-nxt-rxe-file.
"
  (format s "digraph G {~%")
  (loop 
     :with clump-records = (clump-records p)
     :with name = (list)
     :for instr :in (parse-code (code-words (code-words p)))
     :for instr-data = (cdr instr)
     :for address = (car instr)
     :for clump-start = (clump-starts-at (fixed-clump-data clump-records) address)
     :for previous-clump = nil :then current-clump
     :for current-clump =  (if clump-start clump-start current-clump)
     :for clump = (if current-clump (aref (fixed-clump-data clump-records) 
					  current-clump) 
		      nil)
     :for depend = (if current-clump (aref (dependency-lists clump-records) 
					   current-clump)
		       nil)
     :do
     (progn
     (when clump-start
       (when previous-clump
	 (format s "~D [label=\"~D~{\\n~A~}\"];~%" previous-clump previous-clump name))
       (setf name (list))

       (when (eql 0 (fire-count clump))
	 (format s "~D [shape=box];~%" clump-start))
       (loop :for i :in depend :do
	  (format s "~D -> ~D [color=blue];~%" current-clump i)))
     
     (case (getf instr-data :instruction)
       ((subcall s-subcall)
	(format s "~D -> ~D [color=red];~%" current-clump
		(getf instr-data :subroutine )))
       (finclumpimmed
	(format s "~D -> ~D [color=green];~%" current-clump
		(getf instr-data :clumpid)))
       (syscall
	(pushnew "syscall" name :test #'equalp))
       (stop 
	(pushnew "stop" name :test #'equalp))
       (setout
	(pushnew "setout" name :test #'equalp))
       (getout
	(pushnew "getout" name :test #'equalp))
       (getin
	(pushnew "getin" name :test #'equalp))
       (setin
	(pushnew "setin" name :test #'equalp))
       (t nil)))
     :finally             (format s "~D [label=\"~D~{\\n~A~}\"];~%" previous-clump previous-clump name)

     )
  (format s "}~%"))
       
(defun write-dot-file (filename data)
  "Writes the call graph of program `data' to `filename'.
See make-call-graph for more details."
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (make-call-graph data s)))