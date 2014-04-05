
;;;; city-hash.asd

(defsystem city-hash
  :name "City Hash"
  :description "CityHash hash functions"
  :long-description
"A Common Lisp implementation of Google's CityHash family of hash functions.
The Lisp algorithm is identical to that of Google's open source C++ code,
release 1.0.3."
  :version "1.7"
  :author "Robert Brown"
  :license "MIT License.  See the copyright messages in individual files."
  :defsystem-depends-on (com.google.base)
  :depends-on (com.google.base
               nibbles
               #+sbcl sb-rotate-byte)
  :in-order-to ((test-op (test-op city-hash-test)))
  :components
  ((:file "package")
   (:fast-unsafe-source-file "city-hash" :depends-on ("package"))))
