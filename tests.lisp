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

(in-package :cl-user)

(defpackage :madeira-port-tests
  (:use :cl :madeira-port :eos)
  (:export #:run-tests))

(in-package :madeira-port-tests)

(defun feature-eval-tests ()
  (is (feature-eval '(:and)))
  (is (not (feature-eval '(:or))))
  (is (not (feature-eval '(:not (:and)))))
  (is (feature-eval '(:not (:or))))
  (is (feature-eval '(:and :common-lisp :ansi-cl)))
  (is (feature-eval '(:or :common-lisp :ansi-cl)))
  (is (feature-eval '(:or #:cant-be :common-lisp)))
  (is (feature-eval '(:or :common-lisp #:cant-be)))
  (is (not (feature-eval '(:and :common-lisp #:cant-be))))
  (is (not (feature-eval '(:and #:cant-be :common-lisp))))
  (is (equal "foo" (feature-eval '"foo")))
  (is (feature-eval '(:find-symbol #:t :cl)))
  (is (not (feature-eval '(:find-symbol #:tskdfjhkasjfh :cl))))
  (is (feature-eval '(:find-variable #:t :cl)))
  (is (not (feature-eval '(:find-function #:not-this :madeira-port))))
  (is (feature-eval '(:find-function #:feature-eval :madeira-port)))
  (is (not (feature-eval '(:find-macro #:with-open-file :madeira-port))))
  (is (feature-eval '(:find-macro #:with-open-file :madeira-port t)))
  (is (feature-eval '(:find-macro #:with-open-file :common-lisp)))
  (is (feature-eval '(:eq (:find-symbol #:cons :madeira-port t)
                      (:find-symbol #:cons :cl))))
  (is (feature-eval '(:equal "foo" "foo")))
  (is (feature-eval '(:find-package :madeira-port)))
  (is (not (feature-eval '(:find-package :msafkjsdafksdhfksdhfaksldjfhaklsdj))))
  (is (feature-eval '(:typep (:find-function #:add-method :cl) 'generic-function)))
  (is (feature-eval '(:find-class #:string :cl)))
  (is (not (feature-eval '(:find-class #:skfhjsdkfhsdaklfjhsdlkfjhsdsadkfjh :cl))))
  (is (feature-eval '(:typep (:find-value #:*feature-evaluators* :madeira-port t)
                      'hash-table))))

(defun feature-syntax-tests ()
  (let ((*readtable* *readtable*))
    (extend-feature-syntax)
    (is (eq :ok (read-from-string "#+(:find-symbol #:sdkhaskj :cl) :oops :ok")))
    (is (eq :ok (read-from-string "#-(:find-symbol #:sdkhaskj :cl) :ok :oops")))
    (is (eq :ok (read-from-string "#+(:find-symbol #:cons :cl) :ok :oops")))
    (is (eq :ok (read-from-string "#-(:find-symbol #:cons :cl) :oops :ok")))))

(test madeira-port
  (feature-eval-tests)
  (feature-syntax-tests))

(defun run-tests ()
  (let ((results (run 'madeira-port)))
    (explain! results)
    (unless (results-status results)
      (error "Tests failed."))))
