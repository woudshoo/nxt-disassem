;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The initial data content
;;;
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :nxt-disassem)



(defun create-full-dstoc-entry (dstoc-entry static-data-entry 
				dope-vector dynamic-data dynamic-data-offset)
  "Returns the dstoc entry together with the data values filled in.

The mapping is as follows:

  - simple cases
     dstoc-entry --->  (dstoc-entry . value)
  - cluster
     (dstoc-entry sub-entryes...)  ---> (dstoc-entry . converted sub-entries)
  - array
    (dstoc-entry sub-entry) ---> (sub-entry . #(values ..))"

  (let ((entry-type (dstoc-type dstoc-entry)))
    (case entry-type
      (:tc-cluster 
       (cons dstoc-entry (mapcar (lambda (s1 s2) 
				   (create-full-dstoc-entry s1 s2
							    dope-vector dynamic-data
							    dynamic-data-offset)) 
				 (rest dstoc-entry) static-data-entry)))
      (:tc-array 
       (let ((se (second dstoc-entry)))
	 (cons se
	       "not implemented"
	       )))
      (t (cons dstoc-entry 
	       (if (dstoc-initial-content-from-file dstoc-entry)
		   static-data-entry 
		   0))))))


(defun test-create-full (p)
  (loop :for entry :in (parse-dstoc-table (dstoc-table p))
     :for data :in (default-static-data p)
     :collect (create-full-dstoc-entry entry data nil nil nil)))


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