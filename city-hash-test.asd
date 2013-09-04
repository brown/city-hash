
;;;; city-hash-test.asd

(defsystem city-hash-test
  :depends-on (city-hash hu.dwim.stefil)
  :components
  ((:file "city-hash_test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'city-hash-test))))
  (funcall (read-from-string "city-hash-test:test-city-hash")))
