(in-package :cl-user)

(defpackage :hash-table-ext
  (:use :cl)
  (:export))

(in-package :hash-table-ext)

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
;;;; CL ITERATION ANALOGOUS
;;; DOLIST
;;;; CL OBJECT ANALOGOUS
;;; WITH-SLOTS WITH-ACCESSORS
;;;; CL SEQUENCE ANALOGOUS
;;; SUBSEQ

