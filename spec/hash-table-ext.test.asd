; vim: ft=lisp et
(in-package :asdf)
(defsystem "hash-table-ext.test"
  :version
  "0.10.0"
  :depends-on
  (:jingoh "hash-table-ext")
  :components
  ((:file "hash-table-ext"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :hash-table-ext args)))
