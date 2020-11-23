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

(declaim
 (ftype (function
         (hash-table &key (:key (or symbol function))
          (:test (or symbol function)) (:size (integer 0 *))
          (:rehash-size (or (integer 1 *) (float 1.0 *)))
          (:rehash-threshold (real 0 1)))
         (values hash-table &optional))
        copy-ht))

(setf (fdefinition 'copy-ht) #'alexandria:copy-hash-table)

;;; MEMBER MEMBER-IF
;;; MAPC MAPCAR MAPCAN

(declaim
 (ftype (function (function hash-table) (values hash-table &optional))
        mapht
        map-hash-table
        nmapht))

(defun mapht (function hash-table) (maphash function hash-table) hash-table)

(defun map-hash-table (function hash-table)
  (let ((new (copy-ht hash-table)))
    (doht ((k v) hash-table new)
      (setf (gethash k new) (funcall function k v)))))

(defun nmapht (function hash-table)
  (doht ((k v) hash-table hash-table)
    (setf (gethash k hash-table) (funcall function k v))))

;;; PAIRLIS

(declaim
 (ftype (function (list list &optional hash-table)
         (values hash-table &optional))
        pairht))

(defun pairht (keys values &optional (hash-table (make-hash-table)))
  (loop :for k :in keys
        :for v :in values
        :do (setf (gethash k hash-table) v)
        :finally (return hash-table)))

;;; ADJOIN

(declaim
 (ftype (function (t t hash-table) (values hash-table &optional)) ht-adjoin))

(defun ht-adjoin (key value hash-table)
  (unless (nth-value 1 (gethash key hash-table))
    (setf (gethash key hash-table) value))
  hash-table)

;;; ASSOC ASSOC-IF
;;; RASSOC RASSOC-IF
;;; INTERSECTION NINTERSECTION
;;; SET-DIFFERENCE NSET-DIFFERENCE
;;; SET-EXCLUSIVE-OR NSET-EXCLUSIVE-OR
;;; UNION

(declaim
 (ftype (function (hash-table hash-table &optional boolean)
         (values hash-table &optional))
        ht-union))

(defun ht-union (ht1 ht2 &optional (keep t))
  (let ((new (copy-ht ht1)))
    (if keep
        (maphash (lambda (k v) (ht-adjoin k v new)) ht2)
        (maphash (lambda (k v) (setf (gethash k new) v)) ht2))
    new))

;;; SUBSETP
;;;; CL OBJECT ANALOGOUS
;;; WITH-SLOTS WITH-ACCESSORS
;;;; CL SEQUENCE ANALOGOUS
;;; SUBSEQ

