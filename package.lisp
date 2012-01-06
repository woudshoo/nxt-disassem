(in-package :cl-user)

(defpackage :nxt-disassem
  (:use :cl :com.gigamonkeys.binary-data)
  (:shadow :type)
  (:export
   #:parse-nxt-rxe-file
   #:print-assembly
   #:make-call-graph
   #:parse-dstoc-table
   #:clump-records
   #:code-words
   #:format-string
   #:format-version
   #:data-space-header
   #:clump-count
   #:code-word-count
   #:file-header
   #:data-space
   #:dstoc-table
   #:default-static-data
   #:default-dynamic-data
   #:fixed-clump-data
   #:dependency-lists
   #:fire-count
   #:dependency-count
   #:code-start-offset
   #:parse-code))

(pushnew :nxt-disassem *features*)

  