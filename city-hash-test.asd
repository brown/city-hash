
;;;; city-hash-test.asd

(in-package #:common-lisp-user)

(defpackage #:city-hash-test-system
  (:documentation "System definition for testing the CITY-HASH package.")
  (:use #:common-lisp #:asdf))

(in-package #:city-hash-test-system)

(defsystem city-hash-test
  :depends-on (:city-hash #:hu.dwim.stefil)
  :components
  ((:file "city-hash_test")))

(defmethod perform ((operation test-op) (component (eql (find-system :city-hash-test))))
  (funcall (read-from-string "city-hash-test::test-city-hash")))
