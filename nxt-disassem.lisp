(in-package :nxt-disassem)


(load #P"/Users/woudshoo/Development/Source/practicals-1.0.3/Chapter08/packages")
(load #P"/Users/woudshoo/Development/Source/practicals-1.0.3/Chapter08/macro-utilities")

(load #P"/Users/woudshoo/Development/Source/practicals-1.0.3/Chapter24/packages")
(load #P"/Users/woudshoo/Development/Source/practicals-1.0.3/Chapter24/binary-data")

(use-package :com.gigamonkeys.binary-data)

(defparameter *endian* :little-endian)

(defun read-unsigned-int (in bytes)
  (ecase *endian*
    (:big-endian
     (loop :with value = 0
	:for low-bit :downfrom (* 8 (1- bytes)) :to 0 :by 8 
	:do
	(setf (ldb (byte 8 low-bit) value) (read-byte in))
	:finally (return value)))
    (:little-endian
     (loop :with value = 0
	:for low-bit :from 0 :below (* 8 bytes) :by 8 
	:do
	(setf (ldb (byte 8 low-bit) value) (read-byte in))
	:finally (return value)))))

(defun write-unsigned-int (out value bytes)
   (ecase *endian*
	     (:big-endian
	      (loop :for low-bit :downfrom (* 8 (1- bytes)) :to 0 :by 8
		 :do 
		 (write-byte (ldb (byte 8 low-bit) value) out)))
	     (:little-endian
	      (loop :for low-bit :from 0 :below(* 8 bytes) :by 8
		 :do 
		 (write-byte (ldb (byte 8 low-bit) value) out)))))

(define-binary-type unsigned-integer (bytes)
  (:reader (in)
	   (read-unsigned-int in bytes))
	     
  (:writer (out value)
	   (write-unsigned-int out value bytes)))



(define-binary-type fixed-length-string (length)
  (:reader (in)
	   (map 'string #'code-char 
		(loop :repeat length
		   :for byte = (read-byte in)
		   :unless (= 0 byte) :collect byte)))
  (:writer (out value)
	  (loop 
	     :with string-length = (length value)
	     :for index :from 0 :below length 
	     :if (< index string-length) 
	     :do (write-byte (char-code (aref value index)) out)
	     :else
	     :do (write-byte 0 out))))

(define-binary-type unsigned-8 () (unsigned-integer :bytes 1))
(define-binary-type unsigned-16 () (unsigned-integer :bytes 2))
(define-binary-type unsigned-32 () (unsigned-integer :bytes 4))
(define-binary-type unsigned-16-be () 
  (:reader (in) (let ((*endian* :big-endian)) (read-unsigned-int in 2)))
  (:writer (out value) (let ((*endian* :big-endian)) (write-unsigned-int out value 2))))

(define-binary-type padding (align)
  (:reader (in)
	   (loop :while (not (eql 0 (mod (file-position in) align)))
	      :collect (read-value 'unsigned-8 in)))
  (:writer (out value)))


(define-binary-type binary-type-array (type count)
  (:reader (in)
	   (let ((result (make-array count)))
	     (loop :for index :from 0 :below count :do
		(setf (aref result index) (read-value type in)))
	     result))
  (:writer (out values)
	  (loop :for value :across values :do
	     (write-value type out value))))


;"Fields is an assoc list of (value . name)"
(define-binary-type value-field (fields)
  (:reader (in)
	   (let* ((value (read-byte in))
		  (entry (assoc value fields)))
	     (if entry
		 (cdr entry)
		 value)))
  (:writer (out value)
	   (let ((entry (rassoc value fields)))
	     (if entry
		 (write-byte (car entry) out)
		 (write-byte value out)))))
	     
	     
(define-binary-class dstoc-entry ()
  ((type (value-field :fields '((0 . :tc-void)
				(1 . :tc-ubyte)
			       (2 . :tc-sbyte)
			       (3 . :tc-uword)
			       (4 . :tc-sword)
			       (5 . :tc-ulong)
			       (6 . :tc-slong)
			       (7 . :tc-array)
			       (8 . :tc-cluster)
			       (9 . :tc-mutex))))
   (flags (value-field :fields '((0 . :initialize-from-file)
				 (1 . :initialize-with-zero))))
   (data-descriptor unsigned-16)))

(defmethod print-object ((o dstoc-entry) s)
  (print-unreadable-object (o s :type t :identity t)
    (format s "~S ~S ~S" (type o)
	    (flags o)
	    (data-descriptor o))))
	    
  
(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-void)))
  nil)

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-mutex)))
  (when (dstoc-initial-content-from-file toc-entry)
    (read-value 'unsigned-32 in)))

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-ubyte)))
  (when (dstoc-initial-content-from-file toc-entry)
    (read-value 'unsigned-8 in)))

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-sbyte)))
  (when (dstoc-initial-content-from-file toc-entry)
  (read-value 'unsigned-8 in)))

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-uword)))
  (when (dstoc-initial-content-from-file toc-entry)
  (read-value 'unsigned-16 in)))

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-sword)))
  (when (dstoc-initial-content-from-file toc-entry)
  (read-value 'unsigned-16 in)))

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-ulong)))
  (when (dstoc-initial-content-from-file toc-entry)
  (read-value 'unsigned-32 in)))

(defmethod read-static-data ((in stream) (toc-entry dstoc-entry) (type (eql :tc-slong)))
  (when (dstoc-initial-content-from-file toc-entry)
  (read-value 'unsigned-32 in)))
  
(defmethod read-static-data ((in stream) (toc-entries list) (type (eql :tc-array)))
  (read-value 'unsigned-16 in)) ;; dope vector index


(defmethod read-static-data ((in stream) (toc-entries list) (type (eql :tc-cluster)))
  (loop :for entry :in (rest toc-entries)
     :collect (read-static-data in entry (dstoc-type entry))))
  
(defun dstoc-type (dstoc-entry)
  (type (if (listp dstoc-entry) (car dstoc-entry) dstoc-entry)))
  
(defun dstoc-initial-content-from-file (dstoc-entry)
  (eq :initialize-from-file (flags dstoc-entry)))

(define-binary-type default-static-data (dstoc length)
  (:reader (in)
	   (loop :for toc-entry :in dstoc 
	      :collect (read-static-data in toc-entry (dstoc-type toc-entry))))
  (:writer (out value)))

(defun static-length-from-dstoc (parsed-dstoc)
  (if (eql (type (car parsed-dstoc)) :tc-array)
      2
      (loop :for entry :in parsed-dstoc 
	 :sum (if (listp entry) (static-length-from-dstoc entry)
		  (if (dstoc-initial-content-from-file entry)
		      (case (dstoc-type entry)
			((:tc-void :tc-cluster) 0)
			((:tc-sbyte :tc-ubyte) 1)
			((:tc-sword :tc-uword) 2)
			((:tc-slong :tc-ulong :tc-mutex) 4)
			(t (error "unknown type")))
		      0)))))


 (define-binary-class data-space-header ()
  ((dstoc-count unsigned-16)
   (initial-size unsigned-16)
   (static-size unsigned-16)
   (default-data-size unsigned-16)
   (dynamic-default-offset unsigned-16)
   (dynamic-default-size unsigned-16)
   (memory-manager-head unsigned-16)
   (memory-manager-tail unsigned-16)
   (dope-vector-offset unsigned-16)))

(define-binary-class rxe-file-header ()
  ((format-string (fixed-length-string :length 14))
   (format-version unsigned-16-be)
   (data-space-header data-space-header)
   (clump-count unsigned-16)
   (code-word-count unsigned-16)))


(define-binary-class rxe-data-space ()
  ((dstoc-table (binary-type-array 
		 :count (dstoc-count 
			 (data-space-header 
			  (file-header 
			   (parent-of-type 'nxt-rxe-file))))
		 :type 'dstoc-entry))
   (default-static-data (default-static-data
			    :dstoc (parse-dstoc-table 
				    (parent-of-type 'rxe-data-space))
			    :length (dynamic-default-offset
				     (data-space-header
				      (file-header
				       (parent-of-type 'nxt-rxe-file))))))
   (default-dynamic-data (binary-type-array
			  :count (dynamic-default-size 
				  (data-space-header
				   (file-header 
				    (parent-of-type 'nxt-rxe-file))))
			  :type 'unsigned-8))
   (padding (padding :align 2))))



(defmethod parse-dstoc-entry ((entries list))
  (let ((top-entry (pop entries)))
    (case (type top-entry)
      (:tc-array (multiple-value-bind (entry-type rest) 
		     (parse-dstoc-entry entries)
		 (values (list top-entry entry-type) rest)))
      (:tc-cluster 
       (let ((result (list top-entry)))
	 (loop :repeat (data-descriptor top-entry)
	    :do
	    (multiple-value-bind (sub-entry rest) 
		(parse-dstoc-entry entries)
	      (setf entries rest)
	      (push sub-entry result)))
	 (values (nreverse result) entries)))
      (t (values top-entry entries)))))

(defmethod parse-dstoc-table ((data-space rxe-data-space))
  (let ((result (list))
	(entries (coerce (dstoc-table data-space) 'list)))
    (loop :while (> (length entries) 0)
       :do
       (multiple-value-bind (entry rest)
	   (parse-dstoc-entry entries)
	 (setf entries rest)
	 (push entry result)))
    (reverse result)))

(defmethod recursively-map ((data list) (function function))
  (loop :for element :in data
     :collect (if (listp element) 
		  (recursively-map element function)
		  (funcall function  element))))



(define-binary-type clump-dependency-lists (clump-records)
  (:reader (in)
	   (let ((result (make-array (length clump-records))))
	     (loop :for clump-record :across clump-records 
		:for index :from 0
		:do
		(setf (aref result index)
		      (loop :repeat (dependency-count clump-record)
			 :collect (read-value 'unsigned-8 in))))
	     result))
  (:writer (out value)))
		
(define-binary-class clump-record-fixed ()
  ((fire-count unsigned-8)
   (dependency-count unsigned-8)
   (code-start-offset unsigned-16)))

(define-binary-class code-words ()
  ((code-words (binary-type-array :count (code-word-count (file-header (parent-of-type 'nxt-rxe-file)))
				  :type 'unsigned-16))))


(defparameter *short-opcodes*
  '((0 . :mov)
    (1 . :aquire)
    (2 . :release)
    (3 . :subcall)))


(defparameter *long-opcodes*
  '((#x00 . :add)
    (#x01 . :sub)
    (#x02 . :neg)
    (#x03 . :mul)
    (#x04 . :div)
    (#x05 . :mod)
    (#x06 . :and)
    (#x07 . :or)
    (#x08 . :xor)
    (#x09 . :not)
;    (#x0a . :unknown)
    (#x11 . :cmp)
    (#x12 . :tst)
    ;; unknowns
    (#x15 . :index)
    (#x16 . :replace)
    (#x17 . :arrsize)
    (#x18 . :arrbuild)
    (#x19 . :arrsubset)
    (#x1a . :arrinit)
    (#x1b . :mov)
    (#x1c . :set)
    (#x1d . :flatten)
    (#x1e . :unflatten)
    (#x1f . :numtostring)
    (#x20 . :stringtonum)
    (#x21 . :strcat)
    (#x22 . :strsubset)
    (#x23 . :strtobytearr)
    (#x24 . :bytearrtostr)
    
    (#x25 . :jmp)
    (#x26 . :brcmp)
    (#x27 . :brtst)

    (#x28 . :syscall)

    (#x29 . :stop)
    (#x2a . :finclump)
    (#x2b . :finclumpimmed)
    (#x2c . :acquire)
    (#x2d . :release)
    (#x2e . :subcall)
    (#x2f . :subret)

    (#x30 . :setin)
    (#x31 . :setout)
    (#x32 . :getin)
    (#x33 . :getout)
    ;;unknown
    (#x35 . :gettick)))


(defun value-for (value assoc)
  (let ((entry (assoc value assoc)))
    (if entry
	(cdr entry)
	value)))

(defun signed-value (data index)
  (let ((value (aref data index)))
    (if (zerop (ldb (byte 1 15) value))
	value
	(- value #x10000))))

(defmethod split-instruction-into-components ((data vector) (index number))
  (let* ((word-1 (aref data index))
	 (short-format (eql 1 (ldb (byte 1 11) word-1)))
	 (opcode (if short-format 
		     (ldb (byte 3 8) word-1)
		     (ldb (byte 8 0) word-1)))
	 (size (let ((tmp-size (ldb (byte 4 12) word-1)))
		     (if (eql #xe tmp-size)
			 (aref data (1+ index))
			 tmp-size))) ;; ???? ;; how to count bits???
	 (flags (unless short-format 
		  (ldb (byte 3 8) word-1)))
	 (argument-1 (when short-format
		       (let ((tmp (ldb (byte 8 0) word-1)))
			 (if (zerop (ldb (byte 1 7) tmp))
			     tmp
			     (- tmp 256)))
))) ;; how do we count bits??
    (if short-format
	(values (concatenate 'list
			     (list :opcode opcode
				   :short-format short-format
				   :size size
				   :argument-1 argument-1)
			     (cond 
			       ((eql size 2) (list :arguments (list argument-1)))
			       ((eql size 4) 
				(list :arguments
				      (list
				       (+ argument-1 (signed-value data (+ index 1)))
				       (signed-value data (+ index 1)))))
			       (t (error "Unexpected instruction length"))))
		(+ index (/ size 2)))
				   
	(values (concatenate 'list
			     (list :opcode opcode
				   :short-format short-format
				   :size size
				   :flags flags :arguments)
			     (list  (loop :for i :from 1 :below (/ size 2)
				       :collect (signed-value data (+ index i)))))
		(+ index (/ size 2))))))
    
    
(defun opcode-data-to-string (instr)
  (let ((opcode (cdr instr)))
    (format nil "~6D: ~15A ~A" 
	    (car instr)
	    (or (getf opcode :opcode-name)
			    (getf opcode :instruction))
	  (or (getf opcode :arguments)
	      (cddr opcode)))))


(defmethod disassem ((data list) (opcode t) (short t))
  data)

(defmethod disassem ((data list) (opcode number) (short (eql nil)))
  (concatenate 'list
	       (list :opcode-name (value-for opcode *long-opcodes*))
	       data))

(defmethod disassem ((data list) (opcode number) (short (eql t)))
  (concatenate 'list
	       (list :opcode-name (value-for opcode *short-opcodes*))
	       data))


(defun cmp-instr (conf-data data)
  (list (second conf-data) 
	(case (getf data :flags)
	  (0 '<)
	  (1 '>)
	  (2 '<=)
	  (3 '>=)
	  (4 '==)
	  (5 '!=)
	  (t '??))))

(defmacro definstruction (instr code &rest arguments)
  `(defmethod disassem ((data list) (opcode (eql ,code)) (short (eql nil)))
     (let ((arguments (getf data :arguments)))
       ,(let ((result (list)))
	     (push 'list result)
	     (push :instruction result)
	     (push (if (eq 'quote (car instr)) 
		       instr
		       `(,(car instr) ',instr data))
		   result)
	     (loop :for arg :in arguments
		:for i :from 0
		:do
		(if (listp arg)
		    (progn
		      (push (car arg) result)
		      (push `(subseq arguments ,i) result))
		    (progn
		      (push arg result)
		      (push `(nth ,i arguments) result))))
	     (reverse result)))))
			
(defmacro definstruction-short (instr code &rest arguments)
  `(defmethod disassem ((data list) (opcode (eql ,code)) (short (eql t)))
     (let ((arguments (getf data :arguments)))
       ,(let ((result (list)))
	     (push 'list result)
	     (push :instruction result)
	     (push (if (eq 'quote (car instr)) instr
		       `(,(car instr) ',instr data)
		       )
		       result)
	     (loop :for arg :in arguments
		:for i :from 0
		:do
		(if (listp arg)
		    (progn
		      (push (car arg) result)
		      (push `(subseq arguments ,i) result))
		    (progn
		      (push arg result)
		      (push `(nth ,i arguments) result))))
	     (reverse result)))))
			
		 

(definstruction-short 's-mov #x00 :destination :source)
(definstruction-short 's-aquire #x01 :mutexid)
(definstruction-short 's-release #x02 :mutexid)
(definstruction-short 's-subcall #x03 :subroutine :callerid)

(definstruction 'add #x00 :destination :source1 :source2)
(definstruction 'sub #x01 :destination :source1 :source2)
(definstruction 'neg #x02 :destination :source)
(definstruction 'mul #x03 :destination :source1 :source2)
(definstruction 'div #x04 :destination :source1 :source2)
(definstruction 'mod #x05 :destination :source1 :source2)
(definstruction 'and #x06 :destination :source1 :source2)
(definstruction 'or #x07 :destination :source1 :source2)
(definstruction 'xor #x08 :destination :source1 :source2)
(definstruction 'not #x09 :destination :source)


(definstruction (cmp-instr cmp) #x11 :destination :source1 :source2)
(definstruction (cmp-instr tst) #x12 :destination :source1 :source2)

(definstruction 'index   #x15 :destination :source :index)
(definstruction 'replace #x16 :destination :source :index :newval)
(definstruction 'arrsize #x17 :destination :source)
(definstruction 'arrbuild #x18 :instrsize :destination (:source))
(definstruction 'arrsubset #x19 :destination :source :index :count)
(definstruction 'arrinit #x1a :destination :newval :count)
(definstruction 'mov #x1b :destination :source)
(definstruction 'set #x1c :destination :immediate)
(definstruction 'flatten #x1d :destination :source)
(definstruction 'unflatten #x1e :destination :error :source :default)
(definstruction 'numtostring #x1f :destination :source)
(definstruction 'stringtonum #x20 :destination :indexpast :source :index :default)
(definstruction 'strcat #x21 :instrsize :destiniation (:source))
(definstruction 'strsubset #x22 :destiniation :source :index :count)
(definstruction 'strtobytearr #x23 :destiniation :source)
(definstruction 'bytearrtostr #x24 :destiniation :source)

(definstruction 'jmp #x25 :offset)
(definstruction (cmp-instr brcmp) #x26 :offset :source1 :source2)
(definstruction (cmp-instr brtst) #x27 :offset :source)
(definstruction 'stop #x29 :confirm)
(definstruction 'finclump #x2a :start :end)
(definstruction 'finclumpimmed #x2b :clumpid)

(definstruction 'acquire #x2c :mutexid)
(definstruction 'release #x2d :mutexid)
(definstruction 'subcall #x2e :subroutine :callerid)
(definstruction 'subret #x2f :callerid)

(definstruction 'syscall #x28 :syscallid :parmcluster)
(definstruction 'setin #x30 :source :port :propid)
(definstruction 'setout #x31  :instrsize :port/portlist (:propid-source))
(definstruction 'getin #x32 :destiniation :port :propid)
(definstruction 'getout #x33 :destiniation :port :poropid)
(definstruction 'gettick #x35 :destiniation)



(defmethod disassem ((data list) (opcode (eql 0)) (short (eql nil)))
  (let ((arguments (getf data :arguments)))
    (list :instruction 'add :destination (first arguments)
	  :source1 (second arguments)
	  :source2 (third arguments))))

(defmethod parse-code ((data vector))
  (let ((index 0)
	(result (list)))
    (loop :while (< index (length data))
       :for instruction-data = (split-instruction-into-components
				data index)
       :do
       (push (cons index (disassem instruction-data 
				   (getf instruction-data :opcode)
				   (getf instruction-data :short-format)))
	     result)
       (incf index (/ (getf instruction-data :size) 2)))
    (nreverse result)))
	
(defmethod print-object ((o clump-record-fixed) stream) 
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "FC: ~S, DC: ~S, CSO: ~S"
	    (fire-count o)
	    (dependency-count o)
	    (code-start-offset o))))

(define-binary-class clump-records ()
  ((fixed-clump-data (binary-type-array
		      :count (clump-count (file-header (parent-of-type 'nxt-rxe-file)))
		      :type 'clump-record-fixed))
   (dependency-lists  (clump-dependency-lists :clump-records fixed-clump-data))))
    
(define-binary-class nxt-rxe-file ()
  ((file-header (rxe-file-header))
   (data-space (rxe-data-space))
   (clump-records (clump-records))
   (padding (padding :align 2))
   (code-words (code-words))))

(defun test-parse ()
  (with-open-file (s "/Users/woudshoo/Development/nxt/ttt.rxe" :element-type '(unsigned-byte 8)) (read-value 'nxt-rxe-file s)))

(defun test-parse-2 ()
  (with-open-file (s "/Users/woudshoo/Development/Source/Lego/test.rxe" :element-type '(unsigned-byte 8)) (read-value 'nxt-rxe-file s)))

(defun test-parse-3 ()
  (with-open-file (s "/Users/woudshoo/Development/Source/Lego/test2.rxe" :element-type '(unsigned-byte 8)) (read-value 'nxt-rxe-file s)))

(defun test-parse-4 ()
  (with-open-file (s "/Users/woudshoo/Development/Source/Lego/test3.rxe" :element-type '(unsigned-byte 8)) (read-value 'nxt-rxe-file s)))


(defun test-parse-file (file-name)
  (with-open-file (s file-name :element-type '(unsigned-byte 8))
    (read-value 'nxt-rxe-file s)))

(defun clump-starts-at (fixed-clump-records address)
  (position address fixed-clump-records :test #'eql :key #'code-start-offset))

(defun print-assembly (p)
  (loop :for instr :in (parse-code (code-words (code-words p)))
     :for address = (car instr)
     :for opcode = (cdr instr)
     :for clump-start = (clump-starts-at (fixed-clump-data (clump-records p))
					 address)
     :do
     (when clump-start
       (format t "~&~%START OF CLUMP: ~D" clump-start))
     (format t "~&~A" (opcode-data-to-string instr)))
  (format t "~&"))


(defun make-call-graph (p &optional s)
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
  (format s "}~%"))q

       
(defun write-dot-file (filename data)
  (with-open-file (s filename :direction :output :if-exists :supersede)
    (make-call-graph data s)))