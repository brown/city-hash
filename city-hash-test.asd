
;;;; city-hash-test.asd

(defsystem city-hash-test
  :name "City Hash test"
  :description "Test code for package CITY-HASH."
  :version "1.7"
  :author "Robert Brown"
  :license "MIT License.  See the copyright messages in individual files."
  :depends-on (city-hash hu.dwim.stefil)
  :components
  ((:file "city-hash_test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'city-hash-test))))
  (funcall (read-from-string "city-hash-test:test-city-hash")))
