(in-package :nxt-disassem)

(defun signed-value (value width)
  "return signed value from word array.
Need refactoring or a bette rname."
  (if (zerop (ldb (byte 1 (1- width)) value))
      value
      (- value (ash 1 width))))

(defun read-unsigned-int (in bytes)
  (loop :with value = 0
     :for low-bit :from 0 :below (* 8 bytes) :by 8
     :do
     (setf (ldb (byte 8 low-bit) value) (read-byte in))
     :finally (return value)))

(defun read-signed-int (in bytes)
  (signed-value (read-unsigned-int in bytes)
		(ash 1 (* 8 bytes))))

(defun write-unsigned-int (out value bytes)
  (loop :for low-bit :from 0 :below (* 8 bytes) :by 8
     :do
     (write-byte (ldb (byte 8 low-bit) value) out)))

(defun write-signed-int (out value bytes)
  (write-unsigned-int out value bytes))

(define-binary-type unsigned-integer (bytes)
  (:reader (in)        (read-unsigned-int in bytes))
  (:writer (out value) (write-unsigned-int out value bytes)))

(define-binary-type signed-integer (bytes)
  (:reader (in)        (read-signed-int in bytes))
  (:writer (out value) (write-signed-int out value bytes)))

(define-binary-type unsigned-8 () (unsigned-integer :bytes 1))
(define-binary-type signed-8 () (signed-integer :bytes 1))
(define-binary-type unsigned-16 () (unsigned-integer :bytes 2))
(define-binary-type signed-16 () (signed-integer :bytes 2))
(define-binary-type unsigned-32 () (unsigned-integer :bytes 4))
(define-binary-type signed-32 () (signed-integer :bytes 4))
(define-binary-type unsigned-16-be ()
  (:reader (in) (let ((result 0))
		  (setf (ldb (byte 8 8) result) (read-byte in))
		  (setf (ldb (byte 8 0) result) (read-byte in))
		  result))
  (:writer (out value)
	   (write-byte (ldb (byte 8 8) value) out)
	   (write-byte (ldb (byte 8 0) value) out)))

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
