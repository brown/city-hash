
;;;; package.lisp

(in-package #:common-lisp-user)

(defpackage #:city-hash
  (:documentation "An implementation of the CityHash family of hash functions.")
  (:use #:common-lisp #:com.google.base)
  (:export #:city-hash-64
           #:city-hash-64-with-seed
           #:city-hash-64-with-seeds
           #:city-hash-128
           #:city-hash-128-with-seed))
