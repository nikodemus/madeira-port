;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2012.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :madeira-port
  (:use :cl :asdf)
  (:export #:madeira-port
           #:feature-eval
           #:extend-feature-syntax))

(in-package :madeira-port)

;;;; FEATURE-EVAL

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *feature-evaluators* (make-hash-table)))

(defun feature-eval (expr)
  "Returns the result of evaluating the feature expression EXPR using
extended feature evaluation rules:

Symbols evaluate to T if they are present in *FEATURES*, and NIL otherwise.

Non-symbol atoms evaluate to themselves. (Standard feature expressions do no
accept non-symbol atoms at all.)

Conses evaluate depending on the operator in the CAR:

  :AND &rest expressions

    If all EXPRESSIONS evaluate to true, evaluates to the value of the last of
    them. If no EXPRESSIONS are supplied, evaluates to T. Otherwise evaluates
    to NIL.

  :NOT expression

    Evaluates to T if EXPRESSION evaluates to NIL using extended feature
    evaluation rules, NIL otherwise.

  :OR &rest expressions

    Evaluates to the value of the first of EXPRESSIONS that evalutes to true.
    NIL otherwise.

   QUOTE expression

    Evaluates to EXPRESSION.

  :EQ expression1 expression2
  :EQL expression1 expression2
  :EQUAL expression1 expression2
  :EQUALP expression1 expression2

    Evaluate to T if EXPRESSION1 and EXPRESSION2 evaluate to values that
    compare to true using the similarly named Common Lisp predicate. Otherwise
    evaluate to NIL.

  :TYPEP value-expression type-expression

    Evalutes to true if VALUE-EXPRESSION evaluates to a value that is
    an TYPEP of the type-designator TYPE-EXPRESSION evaluates to.

  :FIND-PACKAGE package-name

    Evaluates to the designated package if it exists, NIL otherwise.

  :FIND-SYMBOL symbol-name package-name &optional allow-internal

    Evaluates to the designated symbol if named package exits, and the named
    symbol is an external (or accessible, if ALLOW-INTERNAL is true) symbol in
    it. Otherwise evaluates to NIL.

  :FIND-FUNCTION symbol-name package-name &optional allow-internal

    Evaluates to the designated function if the named package exits, the named
    symbol is an external (or accessible, if ALLOW-INTERNAL is true) symbol in
    it that is bound to a function and is not a macro or a special operator.
    Otherwise evaluates to NIL.

  :FIND-MACRO symbol-name package-name &optional allow-internal

    Evaluates to the designated macro-function if the named package exits, the
    named symbol is an external (or accessible, if ALLOW-INTERNAL is true)
    symbol in it that is bound to a global macro. Otherwise evaluates to NIL.

  :FIND-VARIABLE symbol-name package-name &optional allow-internal

    Evaluates to T if the designated symbol if the named package exits, the
    named symbol is an external (or accessible, if ALLOW-INTERNAL is true) and
    BOUNDP symbol. Otherwise evaluates to NIL.

  :FIND-VALUE symbol-name package-name &optional allow-internal

    Evaluates to the SYMBOL-VALUE of the designated symbol if the named
    package exits, the named symbol is an external (or accessible, if
    ALLOW-INTERNAL is true) and BOUNDP symbol. Otherwise evaluates to NIL.

  :FIND-CLASS symbol-name package-name &optional allow-internal

    Evaluates to class associated with the designated symbol if the named
    package exits, the named symbol is an external (or accessible, if
    ALLOW-INTERNAL is true) symbol in it that has an associated class
    definition. Otherwise evaluates to NIL.

In all of these both SYMBOL-NAME and PACKAGE-NAME can be any string
designators.
"
  (typecase expr
    (cons
     (let ((fname (gethash (car expr) *feature-evaluators*)))
       (if fname
           (apply fname (cdr expr))
           (error "Invalid expression in ~S: ~S" 'featurep expr))))
    (symbol
     (not (null (member expr *features* :test #'eq))))
    (otherwise
     expr)))

(defmacro defeature (name lambda-list &body body)
  (let ((fname (intern (format nil "~A-FEATUREP" name))))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (setf (gethash ',name *feature-evaluators*) ',fname))))

(defeature :and (&rest features)
  (loop with last = t
        for x in features
        while (setf last (feature-eval x))
        finally (return last)))

(defeature :not (feature)
  (not (feature-eval feature)))

(defeature :or (&rest features)
  (some #'feature-eval features))

(defeature :eq (feature1 feature2)
  (eq (feature-eval feature1) (feature-eval feature2)))

(defeature :eql (feature1 feature2)
  (eql (feature-eval feature1) (feature-eval feature2)))

(defeature :equal (feature1 feature2)
  (equal (feature-eval feature1) (feature-eval feature2)))

(defeature :equalp (feature1 feature2)
  (equalp (feature-eval feature1) (feature-eval feature2)))

(defeature :typep (value-expr type-expr)
  (typep (feature-eval value-expr) (feature-eval type-expr)))

(defeature :find-package (name)
  (find-package name))

(defeature quote (expr)
  expr)

(defun get-symbol (symbol-name package-name &optional allow-internal)
  (let ((pkg (find-package package-name)))
    (when pkg
      (multiple-value-bind (sym state)
          (find-symbol (string symbol-name) pkg)
        (when (or allow-internal (eq :external state))
          (values sym t))))))

(defeature :find-symbol (symbol-name package-name &optional allow-internal)
  (values (get-symbol symbol-name package-name allow-internal)))

(defeature :find-function (symbol-name package-name &optional allow-internal)
  (multiple-value-bind (sym ok) (get-symbol symbol-name package-name allow-internal)
    (when (and ok (fboundp sym))
      (unless (or (special-operator-p sym)
                  (macro-function sym))
        (symbol-function sym)))))

(defeature :find-macro (symbol-name package-name &optional allow-internal)
  (multiple-value-bind (sym ok) (get-symbol symbol-name package-name allow-internal)
    (when ok
      (macro-function sym))))

(defeature :find-variable (symbol-name package-name &optional allow-internal)
  (multiple-value-bind (sym ok) (get-symbol symbol-name package-name allow-internal)
    (when ok
      (boundp sym))))

(defeature :find-value (symbol-name package-name &optional allow-internal)
  (multiple-value-bind (sym ok) (get-symbol symbol-name package-name allow-internal)
    (when (and ok (boundp sym))
      (symbol-value sym))))

(defeature :find-class (symbol-name package-name &optional allow-internal)
  (multiple-value-bind (sym ok) (get-symbol symbol-name package-name allow-internal)
    (when ok
      (find-class sym))))

;;;; Readtable support for FEATURE-EVAL.

(defmacro extend-feature-syntax ()
  "Sets *READTABLE* to a copy of the current readtable with #+ and #-
readmacros implemented using the extended feature evaluator provided by
FEATURE-EVAL.

Takes effect at both load and compile-time if processed as a top level form."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%extend-feature-syntax)))

(defun %feature-read (stream char n)
  (declare (ignore n))
  (let ((value (let ((*package* (load-time-value (find-package :keyword)))
                     (*read-suppress* nil))
                 (feature-eval (read stream t nil t)))))
    (unless (ecase char
              (#\+ value)
              (#\- (not value)))
      (let ((*read-suppress* t))
        (read stream t nil t)))
    (values)))

(defun %extend-feature-syntax ()
  (let ((rt (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\+ '%feature-read rt)
    (set-dispatch-macro-character #\# #\- '%feature-read rt)
    (setf *readtable* rt))
  nil)

;;;; ASDF EXTENSION: Selecting files based on features

(defclass madeira-port (cl-source-file)
  ((test :initform nil))
  (:documentation
   "Acts otherwise like a regular ASDF:CL-SOURCE-FILE (aka :FILE), but
loaded and compiled if and only if the associated :WHEN and/or :UNLESS
options evaluate to true under FEATURE-EVAL."))

(defmethod print-object ((port madeira-port) stream)
  (let ((name (find-symbol (string 'name) :asdf)))
    (if (and name (slot-exists-p port name)
             (not (slot-boundp port name)))
        ;; Workaround for potential printing errors.
        (print-unreadable-object (port stream :type t :identity t)
          (princ "(no name)" stream))
        (call-next-method))))

(defmethod shared-initialize :after ((port madeira-port) slots &key when unless)
  (setf (slot-value port 'test)
        (cond ((and when unless)
               `(:and ,when (:not ,unless)))
              (when when)
              (unless `(:not ,unless)))))

(defun test-expr (port)
  (or (slot-value port 'test)
      (warn "~S has no feature conditionals." port)))

(defmethod perform :around ((op load-op) (port madeira-port))
  (when (feature-eval (test-expr port))
    (call-next-method)))

(defmethod perform :around ((op load-source-op) (port madeira-port))
  (when (feature-eval (test-expr port))
    (call-next-method)))

(defmethod perform :around ((op compile-op) (port madeira-port))
  (when (feature-eval (test-expr port))
    (call-next-method)))

;;; Switch package to circumvent package locks on implementations supporting
;;; them -- not that ASDF currently locked, but it might be in the future.
;;;
;;; Importing MADEIRA-PORT to ASDF is necessary for
;;;
;;;  (:MADEIRA-PORT ...)
;;;
;;; syntax to work in defsystems -- which is also the reason we call it
;;; :MADEIRA-PORT, and not just a :PORT-FILE or something nice and short.

(in-package :asdf)

(import 'madeira-port:madeira-port :asdf)
