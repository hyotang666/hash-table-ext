(defpackage :hash-table-ext.spec
  (:use :cl :jingoh :hash-table-ext))
(in-package :hash-table-ext.spec)
(setup :hash-table-ext)

(requirements-about PAIRHT :doc-type function)

;;;; Description:

#+syntax (PAIRHT keys values &optional (hash-table (make-hash-table)))
; => result

;;;; Arguments and Values:

; keys := list, otherwise signal an implementation dependent condition.
#?(PAIRHT :NOT-LIST NIL) :signals CONDITION

; values := list, otherwise signal an implementation dependent condition.
#?(PAIRHT NIL "not list") :signals CONDITION

; hash-table := hash-table, otherwise signal an implementation dependent condition.
#?(pairht nil nil "not hash-table") :signals CONDITION
; If not specified, new hash-table is returned.
#?(PAIRHT NIL NIL)
:satisfies (lambda (result) (equalp result (make-hash-table)))
; If specified, modified by new key value pairs.
#?(PAIRHT '(:A :B) '(1 2) (PAIRHT '(C) '(3)))
:satisfies (lambda (result) (equalp result (pairht '(:a :b c) '(1 2 3))))
#?(let ((ht (pairht '(:a) '(1))))
    (pairht '(:b) '(2) ht)
    (equalp ht (pairht '(:a :b) '(1 2))))
=> T

; result := hash-table
#?(PAIRHT '(:A :B) '(1 2))
:be-the HASH-TABLE

;;;; Affected By:
; none.

;;;; Side-Effects:
; Destructively modify optional argument HASH-TABLE.
#?(let ((ht (pairht '(:a) '(1))))
    (pairht '(:a) '(2) ht)
    ht)
:satisfies (lambda (result) (equalp result (pairht '(:a) '(2))))

;;;; Notes:

;;;; Exceptional-Situations:
; When keys and values are different length, an error is signaled.
#?(PAIRHT '(:A :B) '(1 2 3)) :signals SIMPLE-ERROR

(requirements-about HT-ADJOIN :doc-type function)

;;;; Description:

#+syntax (HT-ADJOIN key value hash-table) ; => result

;;;; Arguments and Values:

; key := t

; value := t

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(HT-ADJOIN T T "not hash-table") :signals CONDITION

; result := hash-table

;;;; Affected By:

;;;; Side-Effects:
; Third argument `HASH-TABLE` may modified.
#?(HT-ADJOIN :A 1 (MAKE-HASH-TABLE))
:satisfies (lambda (result) (equalp result (pairht '(:a) '(1))))
#?(HT-ADJOIN :A 1 (PAIRHT '(:A) '(2)))
:satisfies (lambda (result) (equalp result (pairht '(:a) '(2))))

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about HT-NULL :doc-type function)

;;;; Description:

#+syntax (HT-NULL hash-table) ; => result

#?(HT-NULL (MAKE-HASH-TABLE)) => T
#?(HT-NULL (PAIRHT '(:A) '(1))) => NIL

;;;; Arguments and Values:

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(HT-NULL "not hash-table") :signals CONDITION

; result := boolean

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

