(in-package :cl-user)

(defpackage :hash-table-ext
  (:use :cl)
  (:export))

(in-package :hash-table-ext)

;;;; CL ITERATION ANALOGOUS
;;; DOLIST

(defmacro doht
          (((v-key &optional v-value) hash-form &optional return) &body body)
  (alexandria:with-gensyms (next-entry more? vk vv)
    (multiple-value-bind (body decls)
        (alexandria:parse-body body)
      `(with-hash-table-iterator (,next-entry ,hash-form)
         (loop (multiple-value-bind (,more? ,vk ,vv)
                   (,next-entry)
                 (declare (ignorable ,vk ,vv))
                 (let (,@(when v-key
                           `((,v-key ,vk)))
                       ,@(when v-value
                           `((,v-value ,vv))))
                   ,@decls
                   (if ,more?
                       (tagbody ,@body)
                       (return ,return)))))))))

;;;; CL CONSES ANALOGOUS
;;; NULL

(declaim (ftype (function (hash-table) (values boolean &optional)) ht-null))

(defun ht-null (hash-table) (zerop (hash-table-count hash-table)))

;;; COPY-ALIST
;;; MEMBER MEMBER-IF
;;; MAPC MAPCAR MAPCAN
;;; PAIRLIS
;;; ASSOC ASSOC-IF
;;; RASSOC RASSOC-IF
;;; INTERSECTION NINTERSECTION
;;; SET-DIFFERENCE NSET-DIFFERENCE
;;; SET-EXCLUSIVE-OR NSET-EXCLUSIVE-OR
;;; UNION NUNION
;;; SUBSETP
;;; ADJOIN
;;;; CL OBJECT ANALOGOUS
;;; WITH-SLOTS WITH-ACCESSORS
;;;; CL SEQUENCE ANALOGOUS
;;; SUBSEQ

