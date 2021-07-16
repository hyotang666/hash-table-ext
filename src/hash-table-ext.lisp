(in-package :cl-user)

(defpackage :hash-table-ext
  (:use :cl)
  (:nicknames "HT")
  (:export ;; Iteration
           #:doht
           #:mapht
           #:map-hash-table
           #:nmapht
           ;; Bind
           #:with-gethash
           ;; Construct
           #:pairht
           #:ht-adjoin
           ;; Trivial helpers
           #:ht-null
           #:copy-ht
           ;; As set.
           #:ht-intersection
           #:ht-union
           #:ht-set-difference
           #:ht-set-exclusive-or
           #:subhtp
           #:subht)
  (:export ;; Miscellaneous
           #:left
           #:right))

(in-package :hash-table-ext)

(declaim (optimize speed))

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

;;; MAPC MAPCAR MAPCAN

(declaim
 (ftype (function (function hash-table) (values hash-table &optional))
        mapht
        map-hash-table
        nmapht))

(defun mapht (function hash-table)
  (assert (typep function '(or symbol function))) ; CLISP needs.
  (maphash function hash-table)
  hash-table)

(defun map-hash-table (function hash-table)
  (assert (typep function '(or symbol function))) ; CLISP needs.
  (let ((new (copy-ht hash-table)))
    (doht ((k v) hash-table new)
      (setf (gethash k new) (funcall function k v)))))

(defun nmapht (function hash-table)
  (assert (typep function '(or symbol function))) ; CLISP needs.
  (doht ((k v) hash-table hash-table)
    (setf (gethash k hash-table) (funcall function k v))))

;;; PAIRLIS

(declaim
 (ftype (function (list list &optional hash-table)
         (values hash-table &optional))
        pairht))

(defun pairht (keys values &optional (hash-table (make-hash-table)))
  (assert (typep keys 'list)) ; CLISP needs.
  (assert (typep values 'list)) ; CLISP needs.
  (assert (typep hash-table 'hash-table)) ; CLISP needs.
  (loop :for (k . k-rest) :on keys
        :for (v . r-rest) :on values
        :if (or (and k-rest (not r-rest)) (and r-rest (not k-rest)))
          :do (error "The lists of keys and values are of unequal length.")
        :else
          :do (setf (gethash k hash-table) v)
        :finally (return hash-table)))

;;; ADJOIN

(declaim
 (ftype (function (t t hash-table) (values hash-table &optional)) ht-adjoin))

(defun ht-adjoin (key value hash-table)
  (unless (nth-value 1 (gethash key hash-table))
    (setf (gethash key hash-table) value))
  hash-table)

;;; INTERSECTION UNION

(defun left (a b) (declare (ignore b)) a)

(defun right (a b) (declare (ignore a)) b)

(declaim
 (ftype (function (hash-table hash-table &optional (or symbol function))
         (values hash-table &optional))
        ht-intersection
        ht-union))

(defun ht-intersection (ht1 ht2 &optional (function #'left))
  #+clisp
  (progn
   (assert (typep ht2 'hash-table))
   (assert (typep function '(or symbol function))))
  (let ((new (make-hash-table :test (hash-table-test ht1))))
    (doht ((k1 v1) ht1 new)
      (multiple-value-bind (v2 exists?)
          (gethash k1 ht2)
        (when exists?
          (setf (gethash k1 new)
                  (funcall (coerce function 'function) v1 v2)))))))

(defun ht-union (ht1 ht2 &optional (function #'left))
  #+clisp
  (check-type function (or symbol function))
  (let ((new (copy-ht ht1)))
    (doht ((k2 v2) ht2 new)
      (multiple-value-bind (v1 exists?)
          (gethash k2 new)
        (setf (gethash k2 new)
                (if exists?
                    (funcall (coerce function 'function) v1 v2)
                    v2))))))

;;; SET-DIFFERENCE

(declaim
 (ftype (function (hash-table hash-table) (values hash-table &optional))
        ht-set-difference))

(defun ht-set-difference (ht1 ht2)
  (assert (typep ht2 'hash-table)) ; CLISP needs.
  (let ((new (make-hash-table :test (hash-table-test ht1))))
    (doht ((k1 v1) ht1 new)
      (unless (nth-value 1 (gethash k1 ht2))
        (setf (gethash k1 new) v1)))))

;;; SET-EXCLUSIVE-OR

(declaim
 (ftype (function (hash-table hash-table) (values hash-table &optional))
        ht-set-exclusive-or))

(defun ht-set-exclusive-or (ht1 ht2)
  (let ((new (copy-ht ht1)))
    (doht ((k2 v2) ht2 new)
      (if (nth-value 1 (gethash k2 new))
          (remhash k2 new)
          (setf (gethash k2 new) v2)))))

;;; SUBSETP

(declaim
 (ftype (function (hash-table hash-table) (values boolean &optional)) subhtp))

(defun subhtp (ht1 ht2)
  (assert (typep ht2 'hash-table)) ; CLISP needs.
  (doht ((k1) ht1 t)
    (unless (nth-value 1 (gethash k1 ht2))
      (return nil))))

;;;; CL OBJECT ANALOGOUS
;;; WITH-SLOTS WITH-ACCESSORS

(defmacro with-gethash ((&rest defs) hash-table-form &body body)
  ;; Trivial syntax check.
  (every (lambda (def) (check-type def (cons symbol (cons t null)))) defs)
  (let ((hash-table (gensym "HASH-TABLE"))
        (vars (alexandria:make-gensym-list (list-length defs))))
    ;; The body.
    `(let ((,hash-table ,hash-table-form)
           ,@(mapcar (lambda (def var) `(,var ,(second def))) defs vars))
       (check-type ,hash-table hash-table) ; ECL needs.
       (symbol-macrolet ,(mapcar
                           (lambda (def var)
                             `(,(car def) (gethash ,var ,hash-table)))
                           defs vars)
         ,@body))))

;;;; CL SEQUENCE ANALOGOUS
;;; SUBSEQ

(declaim
 (ftype (function (hash-table &rest t) (values hash-table &optional)) subht))

(defun subht (hash-table &rest keys)
  (let ((new
         (make-hash-table :test (hash-table-test hash-table)
                          :rehash-size (hash-table-rehash-size hash-table)
                          :rehash-threshold (hash-table-rehash-threshold
                                              hash-table))))
    (dolist (key keys new)
      (multiple-value-bind (v exists?)
          (gethash key hash-table)
        (if exists?
            (setf (gethash key new) v)
            (error "Missing hash-key: ~S" key))))))