(in-package :cl-user)

(defpackage :nxt-disassem.system
  (:use :cl :asdf))

(in-package :nxt-disassem.system)

(defsystem :nxt-disassem
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "nxt-basic-binary-types")
	       (:file "nxt-rxe-dstoc-functions")
	       (:file "nxt-rxe-file-format")
	       (:file "nxt-instructions")
	       (:file "nxt-disassem"))
  :depends-on (:com.gigamonkeys.binary-data))
