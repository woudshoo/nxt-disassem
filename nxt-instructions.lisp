;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The disassembly of the Next byte codes is done into two steps.
;;
;;  1. First the vector of words containing the byte code stream
;;     is split into individual instructions.
;;
;;     The individual instructions are split into its individual
;;     components:
;;       - opcode
;;       - short/long format
;;       - flags
;;       - size
;;       - arguments
;;
;;     Lets call this the Opcode Format
;;
;;  2. The opcode format is translated into a more readable format,
;;     the opcode + flags are changed into a readable instruction
;;     name and the arguments are named.
;;
;;     This is done by the disassem function.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :nxt-disassem)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Part 1 - The binary -> Opcode format conversion
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun signed-code-word (data index)
  (signed-value (aref data index) 16))


(defun instr-size (data index)
  "Returns the size of the instruction at (aref data index).
It consults the next word if needed."
  (let ((size (ldb (byte 4 12) (aref data index))))
    (if (eql #xe size)
	(aref data (1+ index))
	size)))

(defun instr-short-format (word)
  "Returns true if the instruction is in short format."
  (eql 1 (ldb (byte 1 11) word)))

(defun instr-flags (word)
  "Returns the flags of opcode word."
  (unless (instr-short-format word)
    (ldb (byte 3 8) word)))

(defmethod split-instruction-into-components ((data vector) (index number))
  "Totally hackish function which split an instruction into its components.

It converts the instruction starting at 'index' in the bytecoe stream
'data' into the Opcode format.

The data vector should be a vector of 16-bit words.

The function returns

  'opcode-format-of-instruction'

To determine where the next function starts, use the :size
field of the returned opcode property list.

E.g.  next instruction start = index + (getf return-value :size) / 2
"
  (let* ((word-1 (aref data index))
	 (short-format (instr-short-format word-1))
	 (opcode (if short-format
		     (ldb (byte 3 8) word-1)
		     (ldb (byte 8 0) word-1)))
	 (size (instr-size data index))
	 (argument-1 (when short-format
		       (signed-value (ldb (byte 8 0) word-1) 8)))

	 ;; result is a property list with at least the following values
	 (result (list :opcode opcode :short-format short-format :size size)))

    (if short-format
	;; Short format
	(concatenate 'list
		     result
		     (case size
		       (2 (list :arguments (list argument-1)))
		       (4 (list :arguments
				(list
				 (+ argument-1 (signed-code-word data (+ index 1)))
				 (signed-code-word data (+ index 1)))))
		       (t (error "Unexpected instruction length"))))
	;; Long format
	(concatenate 'list
		     result
		     (list :flags (instr-flags word-1))
		     (list :arguments (loop :for i :from 1 :below (/ size 2)
			       :collect (signed-code-word data (+ index i))))))))



;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Part 2 - The disassem section
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric disassem (data opcode format)
  (:documentation "Disassembles the data from the Opcode format into a
fully parsed format.
The argument `data' should be in opcode format, 
The argument `opcode' is the opcode of the instruction,
The argument `format' is a boolean which is t for short instructions.

The last two arguments are used for dispatching to the specialized functions
to fully disasemble the instruction stored in `data'.

An instruction in fully parsed format is

  (address . instruction)

Where instruction is a property list containing

-  :instruction   the instruction, which is either
                  a symbol naming the instruction
                  or a list for more complicated instructions
-  argument name  If an instruction has arguments
                  the arguments are part of the property 
                  list with key the argument name
                  and value the value of the argument

So for example a full instruction is:

   (310 :INSTRUCTION NXT-DISASSEM::FINCLUMP :START -1 :END -1)

or 

   (321 :INSTRUCTION (NXT-DISASSEM::BRCMP <) :OFFSET -2 :SOURCE1 4 :SOURCE2 5)
"))

;; Fallback for opcodes that are not known.
(defmethod disassem ((data list) (opcode t) (short t))
  data)

(defmethod disassem ((data list) (opcode number) (short (eql nil)))
  (concatenate 'list
	       (list :opcode-name opcode)
	       data))

(defmethod disassem ((data list) (opcode number) (short (eql t)))
  (concatenate 'list
	       (list :opcode-name opcode)
	       data))


;; Macros for defining instructions
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

;; Helper function to create compare instructions.
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

;; The instructions
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
(definstruction 'strcat #x21 :instrsize :destination (:source))
(definstruction 'strsubset #x22 :destination :source :index :count)
(definstruction 'strtobytearr #x23 :destination :source)
(definstruction 'bytearrtostr #x24 :destination :source)

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
(definstruction 'getin #x32 :destination :port :propid)
(definstruction 'getout #x33 :destination :port :poropid)
(definstruction 'gettick #x35 :destination)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  High level disassem code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod parse-code ((data vector))
  "Takes a byte code vector and returns the fully parsed
instructions."
  (loop
     :for index = 0 :then (+ index (/ (getf instruction-data :size) 2))
     :while (< index (length data))
     :for instruction-data = (split-instruction-into-components	data index)
     :collect
     (cons index (disassem instruction-data
			   (getf instruction-data :opcode)
			   (getf instruction-data :short-format)))))


(defmethod get-targets ((instructions list))
  "Return a list of jmp/branch target addresses.  
This function does not remove duplicates."
  (loop :for instr :in instructions
     :for pp = (car instr)
     :for offset = (getf (rest instr) :offset)
     :when offset
     :collect (+ pp offset)))

(defmethod label-targets ((targets list))
  "Returns a hash-table mapping addresses to label names.
The input of this function is a list of addresses as created by get-targets."
  (let ((result (make-hash-table)))
    (loop :for offset :in (remove-duplicates targets)
       :for index :from 1
       :do
       (setf (gethash offset result) (format nil "L~2,'0D" index)))
    result))