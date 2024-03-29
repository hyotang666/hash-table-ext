; vim: ft=lisp et
(in-package :asdf)
(defsystem "hash-table-ext"
  :version
  "1.0.11"
  :depends-on
  (
   "alexandria" ; Public domain utilities.
   )
  :pathname
  "src/"
  :components
  ((:file "hash-table-ext"))
  :author "SATO Shinichi"
  :license "MIT"
  :description "Tiny extensions for common lisp hash-tables."
  :source-control (:git "git@github.com:hyotang666/hash-table-ext")
  :bug-tracker "https://github.com/hyotang666/hash-table-ext/issues")

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "hash-table-ext").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "hash-table-ext"))))
  (append (call-next-method) '((test-op "hash-table-ext.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "hash-table-ext")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "hash-table-ext"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
