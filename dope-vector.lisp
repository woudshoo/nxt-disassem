(in-package :nxt-disassem)

(defclass dv-entry ()
  ((offset :reader offset :initarg :offset :initform :unknown)
   (element-size :reader element-size :initarg :element-size :initform :unknown)
   (element-count :reader element-count :initarg :element-count :initform :unknown)
   (back-pointer :reader back-pointer :initarg :back-pointer :initform :unknown)
   (link-index :reader link-index :initarg :link-index :initform :unknown)))


(defmethod print-object ((obj dv-entry) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Offset: ~A, Elm-Size: ~A, Elm-Count: ~A, BP: ~A, LI: ~A"
	    (offset obj) (element-size obj) (element-count obj)
	    (back-pointer obj) (link-index obj))))


(defun make-dv-entry (data offset)
  "Create a dv entry based on the bytes at `offset' in  byte array `data'."
  (flet ((word ()
	   "Returns a little endian word value from a vector of bytes `data' stored
at location `offset'."
	   (let ((result 0))
	     (setf (ldb (byte 8 0) result) (aref data offset))
	     (incf offset)
	     (setf (ldb (byte 8 8) result) (aref data offset))
	     (incf offset)
	     result)))

    (make-instance 'dv-entry 
		   :offset (word)
		   :element-size (word)
		   :element-count (word)
		   :back-pointer (word)
		   :link-index (word))))


(defmethod dope-vector ((file nxt-rxe-file))
  "Returns the dope vector of the `file'."
  (let* ((result nil)
	 (dsh (data-space-header file))
	 (dyn-data (default-dynamic-data file)))
    (loop 
       :for dv-start = (- (dope-vector-offset dsh) (static-size dsh)) :then (+ dv-start 10)
       :for entry = (make-dv-entry dyn-data dv-start)
       :for count = (element-count entry) :then (- count 1)
       :do (push entry result)
       :while (> count 1)
       :finally (return (reverse result)))))
