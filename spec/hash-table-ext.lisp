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
#?(let ((result))
    (doht ((k) (pairht '(:a :b) '(1 2)))
      (push k result))
    result)
:satisfies (lambda (result) (null (set-exclusive-or result '(:a :b))))
; You can use `NIL` for k when what you interests is only value.
#?(let (result)
    (doht ((nil v) (pairht '(:a :b) '(1 2)))
      (push v result))
    result)
:satisfies (lambda (result) (null (set-exclusive-or result '(1 2))))

; v-value := symbol, otherwise signals implementation dependent condition.
#?(DOHT ((KEY "not symbol") (MAKE-HASH-TABLE))) :signals CONDITION
; Not evaluated
#?(doht ((key (intern "NOT EVALUATED")) (make-hash-table))) :signals condition
; Bound by each value of hash-table.
#?(let (result)
    (doht ((k v) (pairht '(:a :b) '(1 2)))
      (push (cons k v) result))
    result)
:satisfies (lambda (result) (null (set-exclusive-or result '((:a . 1) (:b . 2)) :test #'equal)))

; hash-form := The form which generates hash-table, otherwise signals implementation dependent condition.
#?(DOHT ((KEY) "not hash-table")) :signals CONDITION

; return := NIL, if specified this form is evaluated at last and return its values.
#?(let (result)
    (doht ((k) (pairht '(:a :b) '(1 2)) (push :last result))
      (push k result))
    result)
:satisfies (lambda (result) (null (set-exclusive-or result '(:a :b :last))))
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
      (return v)))
=> 2
; GOable.
#?(let (result)
    (doht ((nil v) (pairht '(:a :b :c) '(1 2 3)))
      (when (evenp v)
        (go :end))
      (push v result)
      :end)
    result)
:satisfies (lambda (result) (null (set-exclusive-or result '(1 3))))

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

#?(let (result)
    (MAPHT (LAMBDA (&REST ARGS) (push ARGS result)) (PAIRHT '(:A :B) '(1 2)))
    result)
:satisfies (lambda (result) (null (set-exclusive-or result '((:a 1) (:b 2)) :test #'equal)))

;;;; Arguments and Values:

; function := function, otherwise signals implementation dependent condition.
#?(MAPHT "not function" (MAKE-HASH-TABLE)) :signals CONDITION
; FUNCTION should be the ftype as (function (t t)).
; Return values are discarded.

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(MAPHT (LAMBDA (&REST ARGS) (PRINT ARGS)) "not hash-table") :signals CONDITION

; result := hash-table, the second argument.
#?(MAPHT (LAMBDA (&REST ARGS) (PRINT ARGS)) (MAKE-HASH-TABLE))
:be-the HASH-TABLE

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:
; See [Traversal rules and side effect.](http://clhs.lisp.se/Body/03_f.htm)
#?(let ((ht (pairht '(:a) '(1))))
    (mapht (lambda (k v)
             (declare (ignore v))
             (remhash k ht))
           ht))
:satisfies (lambda (result)
             (equalp result (make-hash-table)))

;;;; Exceptional-Situations:

(requirements-about MAP-HASH-TABLE :doc-type function)

;;;; Description:

#+syntax (MAP-HASH-TABLE function hash-table) ; => result

;;;; Arguments and Values:

; function := function, otherwise signals implementation dependent condition.
#?(MAP-HASH-TABLE "not-function" (MAKE-HASH-TABLE)) :signals CONDITION
; FUNCTION should be the ftype as (function (t t)).
; Return values are discarded.

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(MAP-HASH-TABLE #'LIST "not hash-table") :signals CONDITION

; result := hash-table, newly allocated.
#?(MAP-HASH-TABLE (LAMBDA (K V) (DECLARE (IGNORE K)) (1+ V))
                  (PAIRHT '(:A :B) '(1 2)))
:satisfies (lambda (result)
             (equalp result (pairht '(:a :b) '(2 3))))

; The second argument `HASH-TABLE` never modified.
#?(LET ((HT (PAIRHT '(:A :B) '(1 2))))
    (MAP-HASH-TABLE (LAMBDA (K V) (DECLARE (IGNORE K)) (1+ V)) HT)
    HT)
:satisfies (lambda (result)
             (equalp result (pairht '(:a :b) '(1 2))))

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; MAP-HASH-TABLE returned new hash-table which has same keys with argument hash-table.
; And each values are set by return value of first argument FUNCTION.
#?(let ((ht (pairht '(:a) '(0))))
    (values (map-hash-table (lambda (k v)
                              (declare (ignore v))
                              (remhash k ht))
                            ht)
            ht))
:multiple-value-satisfies
(lambda (returned origin)
  (& (equalp returned (pairht '(:a) '(t)))
     (equalp origin (make-hash-table))))

;;;; Exceptional-Situations:

(requirements-about NMAPHT :doc-type function)

;;;; Description:

#+syntax (NMAPHT function hash-table) ; => result

;;;; Arguments and Values:

; function := function, otherwise signals implementation dependent condition.
#?(NMAPHT "not function" (MAKE-HASH-TABLE)) :signals CONDITION
; FUNCTION should be the ftype as (function (t t)).
; Return values are discarded.

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(NMAPHT #'LIST "not hash-table") :signals CONDITION

; result := hash-table
#?(LET ((HT (PAIRHT '(:A :B) '(1 2))))
    (NMAPHT (LAMBDA (K V) (DECLARE (IGNORE K)) (1+ V)) HT))
:satisfies (lambda (result)
             (equalp result (pairht '(:a :b) '(2 3))))

;;;; Affected By:
; none

;;;; Side-Effects:
; Destructively modify second argument.
#?(LET ((HT (PAIRHT '(:A :B) '(1 2))))
    (NMAPHT (LAMBDA (K V) (DECLARE (IGNORE K)) (1+ V)) HT)
    HT)
:satisfies (lambda (result)
             (equalp result (pairht '(:a :b) '(2 3))))

;;;; Notes:
; NMAPHT destructively update the second argument HASH-TABLE with returned value of the
; first argument FUNCTION.
#?(let ((ht (pairht '(:a) '(1))))
    (values (nmapht (lambda (k v)
                      (declare (ignore v))
                      (remhash k ht))
                    ht)
            ht))
:multiple-value-satisfies
(lambda (returned origin)
  (& (eq returned origin)
     (equalp returned (pairht '(:a) '(t)))))

;;;; Exceptional-Situations:

(requirements-about WITH-GETHASH :doc-type function)

;;;; Description:

#+syntax (WITH-GETHASH (&rest defs) hash-table-form &body body) ; => result

;;;; Arguments and Values:

; def+ := (name keyform)

; When malformed, signals implementation dependent condition.
#?(WITH-GETHASH ("not def form")
      (MAKE-HASH-TABLE)) :signals CONDITION
#?(WITH-GETHASH ((LESS-ELEMENT))
      (MAKE-HASH-TABLE)) :signals CONDITION
#?(WITH-GETHASH ((TOO MUCH ELEMENTS))
      (MAKE-HASH-TABLE)) :signals CONDITION

; name := symbol, otherwise signals implementation dependent condition.
#?(WITH-GETHASH (("not symbol" :DUMMY))
      (MAKE-HASH-TABLE)) :signals CONDITION
; Not evaluated.
#?(WITH-GETHASH (((INTERN "Not evaluated") :KEY))
      (MAKE-HASH-TABLE)) :signals CONDITION

; keyworm := form which generates key for hash-table.
#?(LET ((HT (MAKE-HASH-TABLE)))
    (WITH-GETHASH ((NAME :KEY)) HT
      (setf name :value)
      ht))
:satisfies (lambda (result)
             (equalp result (pairht '(:key) '(:value))))
; Evaluated only once.
#?(LET ((HT (MAKE-HASH-TABLE)))
    (WITH-GETHASH ((NAME (PRINC :KEY))) HT
      (setf name :value)
      name))
:outputs "KEY"

; hash-table-form := Form which generates hash-table, otherwise signals implementation dependent condition.
#?(with-gethash ((name :key)) "not hash-table")
:signals CONDITION

; body := Implicit progn.

; result := T

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about HT-INTERSECTION :doc-type function)

;;;; Description:

#+syntax (HT-INTERSECTION ht1 ht2 &optional (function #'left)) ; => result

;;;; Arguments and Values:

; ht1 := hash-table, otherwise signals implementation dependent condition.
#?(HT-INTERSECTION "not hash-table" (MAKE-HASH-TABLE)) :signals CONDITION

; ht2 := hash-table, otherwise signals implementation dependent condition.
#?(HT-INTERSECTION (MAKE-HASH-TABLE) "not hash-table") :signals CONDITION

; function := function, otherwise signals implementation dependent condition.
#?(HT-INTERSECTION (MAKE-HASH-TABLE) (MAKE-HASH-TABLE)
                   "not (or symbol function)") :signals CONDITION
; The default is #'LEFT which keeps ht1 value.
#?(HT-INTERSECTION (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4)))
:satisfies (lambda (result) (equalp result (pairht '(:b) '(2))))
; The FUNCTION shoud be ftype as (function (t t)) which accepts v1 and v2.
#?(HT-INTERSECTION (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4)) #'+)
:satisfies (lambda (result) (equalp result (pairht '(:b) '(5))))

; result := hash-table
#?(HT-INTERSECTION (MAKE-HASH-TABLE) (MAKE-HASH-TABLE))
:satisfies (lambda (result) (equalp result (make-hash-table)))

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about HT-UNION :doc-type function)

;;;; Description:

#+syntax (HT-UNION ht1 ht2 &optional (function #'left)) ; => result

;;;; Arguments and Values:

; ht1 := hash-table, otherwise signals implementation dependent condition.
#?(HT-UNION "not hash-table" (MAKE-HASH-TABLE)) :signals CONDITION

; ht2 := hash-table, otherwise signals implementation dependent condition.
#?(HT-UNION (MAKE-HASH-TABLE) "not hash-table") :signals CONDITION

; function := function, otherwise signals implementation dependent condition.
#?(HT-UNION (MAKE-HASH-TABLE) (MAKE-HASH-TABLE) "not (or symbol function)") :signals CONDITION
; The default is #'LEFT which keeps HT1 value.
#?(HT-UNION (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4)))
:satisfies (lambda (result) (equalp result (pairht '(:a :b :c) '(1 2 4))))
; The FUNCTION shoud be ftype as (function (t t)) which accepts v1 and v2.
#?(HT-UNION (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4)) #'+)
:satisfies (lambda (result) (equalp result (pairht '(:a :b :c) '(1 5 4))))

; result := hash-table

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about HT-SET-DIFFERENCE :doc-type function)

;;;; Description:

#+syntax (HT-SET-DIFFERENCE ht1 ht2) ; => result

;;;; Arguments and Values:

; ht1 := hash-table, otherwise signals implementation dependent condition.
#?(HT-SET-DIFFERENCE "not hash-table" (MAKE-HASH-TABLE)) :signals CONDITION

; ht2 := hash-table, otherwise signals implementation dependent condition.
#?(HT-SET-DIFFERENCE (MAKE-HASH-TABLE) "not hash-table") :signals CONDITION

; result := hash-table
#?(HT-SET-DIFFERENCE (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4)))
:satisfies (lambda (result) (equalp result (pairht '(:a) '(1))))

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about HT-SET-EXCLUSIVE-OR :doc-type function)

;;;; Description:

#+syntax (HT-SET-EXCLUSIVE-OR ht1 ht2) ; => result

;;;; Arguments and Values:

; ht1 := hash-table, otherwise signals implementation dependent condition.
#?(HT-SET-EXCLUSIVE-OR "not hash-table" (MAKE-HASH-TABLE)) :signals CONDITION

; ht2 := hash-table, otherwise signals implementation dependent condition.
#?(HT-SET-EXCLUSIVE-OR (MAKE-HASH-TABLE) "not hash-table") :signals CONDITION

; result := hash-table
#?(HT-SET-EXCLUSIVE-OR (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4)))
:satisfies (lambda (result) (equalp result (pairht '(:a :c) '(1 4))))

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SUBHTP :doc-type function)

;;;; Description:

#+syntax (SUBHTP ht1 ht2) ; => result

;;;; Arguments and Values:

; ht1 := hash-table, otherwise signals implementation dependent condition.
#?(SUBHTP "not hash-table" (MAKE-HASH-TABLE)) :signals CONDITION

; ht2 := hash-table, otherwise signals implementation dependent condition.
#?(SUBHTP (MAKE-HASH-TABLE) "not hash-table") :signals CONDITION

; result := boolean
#?(SUBHTP (MAKE-HASH-TABLE) (MAKE-HASH-TABLE)) => T
#?(SUBHTP (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:A :B) '(3 4))) => T
#?(SUBHTP (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:A :B :C) '(3 4 5))) => T
#?(SUBHTP (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:C :D) '(3 4))) => NIL
#?(SUBHTP (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B :C) '(3 4))) => NIL
#?(SUBHTP (PAIRHT '(:A :B) '(1 2)) (PAIRHT '(:B) '(3))) => NIL

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about SUBHT :doc-type function)

;;;; Description:

#+syntax (SUBHT hash-table &rest keys) ; => result

;;;; Arguments and Values:

; hash-table := hash-table, otherwise signals implementation dependent condition.
#?(SUBHT "not hash-table") :signals CONDITION

; keys := t

; result := hash-table
#?(SUBHT (PAIRHT '(:A :B) '(1 2)) :A)
:satisfies (lambda (result) (equalp result (pairht '(:a) '(1))))
#?(SUBHT (PAIRHT '(:A :B :C) '(1 2 3)) :A :C)
:satisfies (lambda (result) (equalp result (pairht '(:a :c) '(1 3))))

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:

;;;; Exceptional-Situations:
; When specified key is missing, an error is signaled.
#?(SUBHT (PAIRHT '(:A :B :C) '(1 2 3)) :A :NO-SUCH-KEY) :signals SIMPLE-ERROR
