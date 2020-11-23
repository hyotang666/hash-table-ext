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

(requirements-about DOHT :doc-type function)

;;;; Description:

#+syntax (DOHT ((v-key &optional v-value) hash-form &optional return)
           &body
           body)
; => result

;;;; Arguments and Values:

; v-key := symbol, otherwise signals implementation dependent condition.
#?(DOHT (("not symbol") (MAKE-HASH-TABLE))) :signals CONDITION
; Not evaluated.
#?(doht ((intern "NOT EVALUATED")) (make-hash-table)) :signals condition
; Bound by each keys of hash-table.
#?(doht ((k) (pairht '(:a :b) '(1 2)))
    (princ k))
:outputs "AB"
; You can use `NIL` for k when what you interests is only value.
#?(doht ((nil v) (pairht '(:a :b) '(1 2)))
    (princ v))
:outputs "12"

; v-value := symbol, otherwise signals implementation dependent condition.
#?(DOHT ((KEY "not symbol") (MAKE-HASH-TABLE))) :signals CONDITION
; Not evaluated
#?(doht ((key (intern "NOT EVALUATED")) (make-hash-table))) :signals condition
; Bound by each value of hash-table.
#?(doht ((k v) (pairht '(:a :b) '(1 2)))
    (format t "~A~A" k v))
:outputs "A1B2"

; hash-form := The form which generates hash-table, otherwise signals implementation dependent condition.
#?(DOHT ((KEY) "not hash-table")) :signals CONDITION

; return := NIL, if specified this form is evaluated at last and return its values.
#?(doht ((k) (pairht '(:a :b) '(1 2)) (princ :last))
    (princ k))
:outputs "ABLAST"
; In the RETURN form, variable v-key and v-value is visible but NIL.
#?(doht((k v) (pairht '(:a) '(1)) (list k v))
    (princ (list k v)))
=> (NIL NIL)
,:stream nil
,:test equal

; body := implicit progn.
; Declarable.
#?(doht ((k) (pairht '(:a) '(1)))
    (declare (type symbol k))
    (princ k))
:outputs "A"
; Returnable.
#?(doht ((nil v) (pairht '(:a :b :c) '(1 2 3)))
    (if (evenp v)
      (return nil)
      (princ v)))
:outputs "1"
; GOable.
#?(doht ((nil v) (pairht '(:a :b :c) '(1 2 3)))
    (when (evenp v)
      (go :end))
    (princ v)
    :end)
:outputs "13"

; result := NIL as default.
#?(doht ((k) (make-hash-table))
    (princ k))
=> NIL
; If RETURN is specified all values are returned.
#?(doht ((k v) (make-hash-table) (values k v))
    (format t "~A~A" k v))
:values (NIL NIL)

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:
; See [Traversal rules and side effect.](http://clhs.lisp.se/Body/03_f.htm)

;;;; Exceptional-Situations:
(requirements-about MAPHT :doc-type function)

;;;; Description:

#+syntax (MAPHT function hash-table) ; => result

#?(MAPHT (LAMBDA (&REST ARGS) (PRINT ARGS)) (PAIRHT '(:A :B) '(1 2)))
:outputs "
(:A 1) 
(:B 2) "

;;;; Arguments and Values:

; function := function, otherwise signals implementation dependent condition.
#?(MAPHT "not function" (MAKE-HASH-TABLE)) :signals CONDITION
; FUNCTION should be the ftype as (function (t t)).
; Return values are discarded.

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(MAPHT (LAMBDA (&REST ARGS) (PRINT ARGS)) "not hash-table") :signals CONDITION

; result := hash-table
#?(MAPHT (LAMBDA (&REST ARGS) (PRINT ARGS)) (MAKE-HASH-TABLE))
:be-the HASH-TABLE

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:
; See [Traversal rules and side effect.](http://clhs.lisp.se/Body/03_f.htm)

;;;; Exceptional-Situations:
