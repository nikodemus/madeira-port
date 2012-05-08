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

(defsystem :madeira-port
  :author "Nikodemus Siivola <nikodemus@random-state.net>"
  :version "1.0"
  :licence "MIT"
  :description
  "Provides :MADEIRA-PORT file class for ASDF, and an extended #+ and #- syntax."
  :components
  ((:file "madeira-port")))

(defsystem :madeira-port-tests
  :licence "MIT"
  :description "Tests for MADEIRA-PORT."
  :depends-on (:madeira-port :eos)
  :components
  ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :madeira-port))))
  (load-system :madeira-port-tests :force '(:madeira-port-tests))
  (funcall (intern "RUN-TESTS" :madeira-port-tests)))
