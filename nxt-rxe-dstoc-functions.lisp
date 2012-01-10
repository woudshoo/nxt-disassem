(in-package :nxt-disassem)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DSTOC helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dstoc-primary-entry (dstoc-entry)
  "If a dstoc-entry is an aggregation type, returns
the entry indicating what type of aggregation it is, instead
of the complete entry.   For non aggergation tyep entries
return the `dstoc-entry' as is."
  (if (listp dstoc-entry) (car dstoc-entry) dstoc-entry))

(defun dstoc-type (dstoc-entry)
  "Returns the type of the dstoc-entry.  The dstoc-entry is either
a primitive entry of class dstoc-entry, or a aggregate type, in which case
the dstoc-entry is a list.  In either case, return the type of the element
which will be parsed."
  (type (dstoc-primary-entry dstoc-entry)))

(defun dstoc-initial-content-from-file (dstoc-entry)
  "Return t if the dstoc-entry will be initialized from the executable
file from the static data section."
  (eq :initialize-from-file
      (flags (dstoc-primary-entry dstoc-entry))))


(defun parse-first-dstoc-entry (entries)
  "Given a list of dstoc entries, parse the first entry completely .
Returning the parsed entry and the unparsed rest . If the first entry
is a primitive type, it just returns the first entry and (rest
entries) . If the first entry is a composite type, parse the complete
composite type, including its components and return the tree
description of the composite type with the unparsed remainder of the
entries list."
  (let ((top-entry (pop entries)))
    (case (type top-entry)

      (:tc-array (multiple-value-bind (entry-type rest)
		     (parse-first-dstoc-entry entries)
		 (values (list top-entry entry-type) rest)))

      (:tc-cluster
       (let ((result (list top-entry)))
	 (loop :repeat (data-descriptor top-entry)
	    :do
	    (multiple-value-bind (sub-entry rest)
		(parse-first-dstoc-entry entries)
	      (setf entries rest)
	      (push sub-entry result)))
	 (values (nreverse result) entries)))

      (t (values top-entry entries)))))

(defun parse-dstoc-table (dstoc-table)
  "Return the tree of parsed dstoc entries stored in data-space.
This function takes a flat list of dstoc entries as argument 
and returns the dstoc entries in tree form.  
In the tree form representation the aggregation entries 
will be represented by one entrie (which in turn describes
its components."
  (let ((result (list))
	(entries (coerce dstoc-table 'list)))
    (loop :while (> (length entries) 0)
       :do
       (multiple-value-bind (entry rest)
	   (parse-first-dstoc-entry entries)
	 (setf entries rest)
	 (push entry result)))
    (reverse result)))
