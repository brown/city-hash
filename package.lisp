
;;;; package.lisp

(in-package #:common-lisp-user)

(defpackage #:city-hash
  (:documentation "An implementation of the CityHash family of hash functions.")
  ;; XXXX: Base is a package defined in the protobuf repository.  Remove the
  ;; dependency or break base out of protobuf.
  (:use #:common-lisp #:base)
  (:export #:city-hash-64
           #:city-hash-64-with-seed
           #:city-hash-64-with-seeds
           #:city-hash-128
           #:city-hash-128-with-seed))
