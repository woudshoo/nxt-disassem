(in-package :nxt-disassem)


(define-binary-class nxt-rxe-file ()
  ((file-header        rxe-file-header)
   (data-space         rxe-data-space)
   (clump-records      clump-records)
   (padding            (padding :align 2))
   (code-words         code-words)))

(define-binary-class rxe-file-header ()
  ((format-string        (fixed-length-string :length 14))
   (format-version        unsigned-16-be)
   (data-space-header     data-space-header)
   (clump-count           unsigned-16)
   (code-word-count       unsigned-16)))

(define-binary-class data-space-header ()
  ((dstoc-count           unsigned-16)
   (initial-size          unsigned-16)
   (static-size           unsigned-16)
   (default-data-size     unsigned-16)
   (dynamic-default-offset unsigned-16)
   (dynamic-default-size  unsigned-16)
   (memory-manager-head   unsigned-16)
   (memory-manager-tail   unsigned-16)
   (dope-vector-offset    unsigned-16)))


(define-binary-class rxe-data-space ()
  ((dstoc-table         (binary-type-array
			 :count (dstoc-count
				 (data-space-header
				  (file-header
				   (parent-of-type 'nxt-rxe-file))))
            		 :type 'dstoc-entry))
   (default-static-data (default-static-data
			    :dstoc-tree (parse-dstoc-table
					 (dstoc-table
					  (parent-of-type 'rxe-data-space)))))
   (default-dynamic-data (binary-type-array
			  :count (dynamic-default-size
				  (data-space-header
				   (file-header
				    (parent-of-type 'nxt-rxe-file))))
			  :type 'unsigned-8))
   (padding              (padding :align 2))))



(define-binary-class code-words ()
  ((code-words (binary-type-array :count (code-word-count
					  (file-header
					   (parent-of-type 'nxt-rxe-file)))
				  :type 'unsigned-16))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DSTOC and STATIC data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



(defun read-static-data-entry (in entry)
  (when (dstoc-initial-content-from-file entry)
    (case (dstoc-type entry)
      (:tc-void               nil)
      (:tc-ubyte             (read-value 'unsigned-8 in))
      (:tc-sbyte             (read-value 'signed-8 in))
      ((:tc-uword :tc-array) (read-value 'unsigned-16 in))
      (:tc-sword             (read-value 'signed-16 in))
      ((:tc-ulong :tc-mutex) (read-value 'unsigned-32 in))
      (:tc-slong             (read-value 'signed-32 in))
      (:tc-cluster           (loop :for c-entry :in (rest entry)
				:collect (read-static-data-entry in c-entry)))
      (t                     (error "Unexpected type in dstoc")))))


(define-binary-type default-static-data (dstoc-tree)
  (:reader (in)
	   (loop :for toc-entry :in dstoc-tree
	      :collect (read-static-data-entry in toc-entry)))
  (:writer (out value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLUMP RECORDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-binary-class clump-records ()
  ((fixed-clump-data (binary-type-array
		      :count (clump-count (file-header (parent-of-type 'nxt-rxe-file)))
		      :type 'clump-record-fixed))
   (dependency-lists  (clump-dependency-lists :clump-records fixed-clump-data))))

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
  ((fire-count        unsigned-8)
   (dependency-count  unsigned-8)
   (code-start-offset unsigned-16)))
