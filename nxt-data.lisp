;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The initial data content
;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :nxt-disassem)


(defun create-full-dstoc-entry (dstoc-entry data-area dope-vector &optional (offset 0))
  "Takes a dstoc entry and returns (dstoc-entry . initial-value).
Note that this most likely will not work correctly yet for
arrays inside arrays.

As another side note, this should be replaced/merged
with the dstoc parsing code."
  (labels 
      ((read-value (location width)
	 (let ((result 0))
	   (loop :repeat width
	      :for index :from 0
	      :for loc :from (+ location offset)
	      :do
	      (setf (ldb (byte 8 (* 8 index)) result) (aref data-area loc)))
	   result)))
    
    (case (dstoc-type dstoc-entry)
      ((:tc-ubyte :tc-sbyte) (cons dstoc-entry (read-value (data-descriptor dstoc-entry) 1)))
      ((:tc-uword :tc-sword) (cons dstoc-entry (read-value (data-descriptor dstoc-entry) 2)))
      ((:tc-ulong :tc-slong :tc-mutex) (cons dstoc-entry (read-value (data-descriptor dstoc-entry) 4)))
      ((:tc-cluster) (cons (first dstoc-entry) 
			 (loop :for entry :in (rest dstoc-entry) :collect (create-full-dstoc-entry entry data-area dope-vector))))
      ((:tc-array) (cons (first dstoc-entry)
			 (or nil (loop 
				    :with entry = (second dstoc-entry)
				    :with dv-index = (read-value (data-descriptor (first dstoc-entry)) 2)
				    :with dv = (nth dv-index dope-vector)
				    :repeat (element-count dv)
				    :for entry-loc = (offset dv) :then (+ entry-loc (element-size dv))
				    :collect (create-full-dstoc-entry entry data-area dope-vector entry-loc))))))))



(defun test-create-full (p)
  (loop
     :with data = (create-data-area p)
     :with dope-vector = (dope-vector p)
     :for entry :in (parse-dstoc-table (dstoc-table p))
     :collect (create-full-dstoc-entry entry data dope-vector)))


(defun create-data-area (p)
  "Returns the initial data area as it will appear in the NXT during running
of the RXE program `p'."
  (let* ((dsh (data-space-header p))
	 (result (make-array (initial-size dsh) :initial-element nil)))

    (labels ((write-value (dstoc-entry value index width)
	       (let ((real-value (if (dstoc-initial-content-from-file dstoc-entry)
				     value 0)))

		 (loop :for byte :from 0 :below width
		    :for location :from index
		    :do
		    (setf (aref result location) (ldb (byte 8 (* 8 byte)) real-value)))
		 ))
	     (write-dstoc-in-memory (dstoc-entry data-entry)
	       (case (dstoc-type dstoc-entry)
		 ((:tc-ubyte :tc-sbyte)
		  (write-value dstoc-entry data-entry (data-descriptor dstoc-entry) 1))
		 ((:tc-uword :tc-sword)
		  (write-value dstoc-entry data-entry (data-descriptor dstoc-entry) 2))
		 (:tc-array 
		  (write-value (car dstoc-entry) data-entry (data-descriptor (car dstoc-entry)) 2))
		 ((:tc-ulong :tc-slong :tc-mutex)
		  (write-value dstoc-entry data-entry (data-descriptor dstoc-entry) 4))
		 (:tc-cluster 
		  (loop :for se :in (rest dstoc-entry)
		     :for sd :in data-entry
		     :do (write-dstoc-in-memory se sd))))))

      (loop :for dstoc-entry :in (parse-dstoc-table (dstoc-table p))
	 :for data :in (default-static-data p)
	 :do
	 (write-dstoc-in-memory dstoc-entry data)))

    (loop :for byte :across (default-dynamic-data p)
       :for index :from (static-size dsh)
       :do
       (setf (aref result index) byte))
    result))