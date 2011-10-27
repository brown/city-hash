
;;;; city-hash.asd

(in-package #:common-lisp-user)

(defpackage #:city-hash-system
  (:documentation "System definition for the CITY-HASH package.")
  (:use #:common-lisp #:asdf))

(in-package #:city-hash-system)

(defsystem city-hash
  :name "City Hash"
  :description "CityHash hash functions"
  :long-description "A Common Lisp implementation of Google's CityHash family of hash functions.
The Lisp algorithm is identical to that of Google's open source C++ code, release 1.0.3."
  :version "1.4"
  :author "Robert Brown"
  :license "MIT License.  See the copyright messages in individual files."
  :depends-on (com.google.base #+sbcl sb-rotate-byte)
  :in-order-to ((test-op (test-op city-hash-test)))
  :components
  ((:file "package")
   (:file "city-hash" :depends-on ("package"))))
