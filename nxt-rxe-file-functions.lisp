(in-package :nxt-disassem)

(defmethod data-space-header ((file nxt-rxe-file))
  (data-space-header (file-header file)))

(defmethod dstoc-table ((file nxt-rxe-file))
  (dstoc-table (data-space file)))

(defmethod default-static-data ((file nxt-rxe-file))
  (default-static-data (data-space file)))

(defmethod default-dynamic-data ((file nxt-rxe-file))
  (default-dynamic-data (data-space file)))

#+nil (defmethod runtime-static-initial-memory ((file nxt-rxe-file))
  (static-data-initial-memory (parse-dstoc-table (dstoc-table file)) (default-static-data file)))

