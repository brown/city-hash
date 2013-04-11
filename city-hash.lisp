;;;; Copyright 2011 Google, Inc.

;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.

;;;; CityHash version 1, by Geoff Pike and Jyrki Alakuijala.
;;;; Translated into Common Lisp by Robert Brown (robert.brown@gmail.com).

(in-package #:city-hash)
(declaim #.*optimize-fast-unsafe*)

;; Random primes between 2^63 and 2^64.
(defconst +k+ #x9ddfea08eb382d69)
(defconst +k0+ #xc3a5c85c97cb3127)
(defconst +k1+ #xb492b66fbe98f273)
(defconst +k2+ #x9ae16a3b2f90404f)
(defconst +k3+ #xc949d7c7509e6557)

(defconst +empty-octet-vector+ (make-octet-vector 0))

(defmacro mod-2^64 (x) `(logand ,x #xffffffffffffffff))

(defmacro u64+ (x y) `(mod-2^64 (+ ,x ,y)))
(defmacro u64- (x y) `(mod-2^64 (- ,x ,y)))
(defmacro u64* (x y) `(mod-2^64 (* ,x ,y)))

(defmacro incf64 (place delta) `(setf ,place (u64+ ,place ,delta)))

(declaim (inline rotate-right-64))

(defun rotate-right-64 (x shift)
  (declare (type uint64 x)
           (type (integer 7 53) shift))
  #+sbcl
  (sb-rotate-byte:rotate-byte (- shift) (byte 64 0) x)
  #-sbcl
  (logior (ash x (- shift)) (mod-2^64 (ash x (- 64 shift)))))

(declaim (inline shift-mix))

(defun shift-mix (x)
  (declare (type uint64 x))
  (logxor x (ash x -47)))

(declaim (inline hash-length-16))

(defun hash-length-16 (u v)
  (declare (type uint64 u v))
  (let* ((a (shift-mix (u64* (logxor u v) +k+)))
         (b (shift-mix (u64* (logxor v a) +k+))))
    (u64* b +k+)))

(defun hash-length-0-to-16 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index length))
  (cond ((> length 8)
         (let ((a (ub64ref/le octets index))
               (b (ub64ref/le octets (+ index length -8))))
           (logxor (hash-length-16 a (rotate-right-64 (u64+ b length) length)) b)))
        ((>= length 4)
         (let ((a (ub32ref/le octets index)))
           (hash-length-16 (u64+ length (ash a 3))
                           (ub32ref/le octets (+ index length -4)))))
        ((plusp length)
         (let* ((a (aref octets index))
                (b (aref octets (+ index (ash length -1))))
                (c (aref octets (+ index length -1)))
                (y (+ a (ash b 8)))
                (z (+ length (ash c 2))))
           (u64* (shift-mix (logxor (u64* y +k2+) (u64* z +k3+))) +k2+)))
        (t +k2+)))

(defun hash-length-17-to-32 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index length))
  (let ((a (u64* (ub64ref/le octets index) +k1+))
        (b (ub64ref/le octets (+ index 8)))
        (c (u64* (ub64ref/le octets (+ index length -8)) +k2+))
        (d (u64* (ub64ref/le octets (+ index length -16)) +k0+)))
    (hash-length-16 (u64+ (u64+ (rotate-right-64 (u64- a b) 43) (rotate-right-64 c 30)) d)
                    (u64+ (u64- (u64+ a (rotate-right-64 (logxor b +k3+) 20)) c) length))))

(declaim (inline weak-hash-length-32-with-seeds))

(defun weak-hash-length-32-with-seeds (octets index a b)
  (declare (type octet-vector octets)
           (type vector-index index)
           (type uint64 a b))
  (let ((w (ub64ref/le octets index))
        (x (ub64ref/le octets (+ index 8)))
        (y (ub64ref/le octets (+ index 16)))
        (z (ub64ref/le octets (+ index 24))))
    (incf64 a w)
    (setf b (rotate-right-64 (u64+ (u64+ b a) z) 21))
    (let ((c a))
      (incf64 a x)
      (incf64 a y)
      (incf64 b (rotate-right-64 a 44))
      (values (u64+ a z) (u64+ b c)))))

(defun hash-length-33-to-64 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index length))
  (let* ((z (ub64ref/le octets (+ index 24)))
         (a (u64+ (ub64ref/le octets index)
                  (u64* (u64+ length (ub64ref/le octets (+ index length -16))) +k0+)))
         (b (rotate-right-64 (u64+ a z) 52))
         (c (rotate-right-64 a 37)))
    (incf64 a (ub64ref/le octets (+ index 8)))
    (incf64 c (rotate-right-64 a 7))
    (incf64 a (ub64ref/le octets (+ index 16)))
    (let ((vf (u64+ a z))
          (vs (u64+ (u64+ b (rotate-right-64 a 31)) c)))
      (setf a (u64+ (ub64ref/le octets (+ index 16)) (ub64ref/le octets (+ index length -32))))
      (setf z (ub64ref/le octets (+ index length -8)))
      (setf b (rotate-right-64 (u64+ a z) 52))
      (setf c (rotate-right-64 a 37))
      (incf64 a (ub64ref/le octets (+ index length -24)))
      (incf64 c (rotate-right-64 a 7))
      (incf64 a (ub64ref/le octets (+ index length -16)))
      (let* ((wf (u64+ a z))
             (ws (u64+ (u64+ b (rotate-right-64 a 31)) c))
             (r (shift-mix (u64+ (u64* (u64+ vf ws) +k2+) (u64* (u64+ wf vs) +k0+)))))
        (u64* (shift-mix (u64+ (u64* r +k0+) vs)) +k2+)))))

(declaim (ftype (function (octet-vector &key (:start vector-index) (:end vector-index))
                          (values uint64 &optional))
                city-hash-64))

(defun city-hash-64 (octets &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8) from index START
to index END and returns the 64-bit hash value as an (UNSIGNED-BYTE 64).  START
defaults to zero, while END defaults to the length of OCTETS."
  (declare (type octet-vector octets)
           (type vector-index start end))
  (let ((length (- end start)))
    (declare (type vector-index length))
    (cond ((<= length 32)
           (if (<= length 16)
               (hash-length-0-to-16 octets start length)
               (hash-length-17-to-32 octets start length)))
          ((<= length 64)
           (hash-length-33-to-64 octets start length))
          (t
           ;; For strings over 64 bytes we hash the end first, and then as we loop we keep 56 bytes
           ;; of state: x, y, z, vf, vs, wf, and ws.
           (let ((x (ub64ref/le octets (- end 40)))
                 (y (u64+ (ub64ref/le octets (- end 16)) (ub64ref/le octets (- end 56))))
                 (z (hash-length-16 (u64+ (ub64ref/le octets (- end 48)) length)
                                    (ub64ref/le octets (- end 24)))))
             (multiple-value-bind (vf vs)
                 (weak-hash-length-32-with-seeds octets (- end 64) length z)
               (multiple-value-bind (wf ws)
                   (weak-hash-length-32-with-seeds octets (- end 32) (u64+ y +k1+) x)
                 (setf x (u64+ (u64* x +k1+) (ub64ref/le octets start)))
                 ;; Decrease length to the nearest multiple of 64, and operate on 64-byte chunks.
                 (setf length (logand (1- length) (lognot 63)))
                 (loop for index from start by 64
                       do (setf x (u64* (rotate-right-64
                                         (u64+ (u64+ (u64+ x y) vf)
                                               (ub64ref/le octets (+ index 8)))
                                         37)
                                        +k1+))
                          (setf y (u64* (rotate-right-64
                                         (u64+ (u64+ y vs) (ub64ref/le octets (+ index 48)))
                                         42)
                                        +k1+))
                          (setf x (logxor x ws))
                          (incf64 y (u64+ vf (ub64ref/le octets (+ index 40))))
                          (setf z (u64* (rotate-right-64 (u64+ z wf) 33) +k1+))
                          (setf (values vf vs)
                                (weak-hash-length-32-with-seeds
                                 octets index (u64* vs +k1+) (u64+ x wf)))
                          (setf (values wf ws)
                                (weak-hash-length-32-with-seeds
                                 octets (+ index 32) (u64+ z ws)
                                 (u64+ y (ub64ref/le octets (+ index 16)))))
                          (rotatef z x)
                          (decf length 64)
                       while (not (zerop length)))
                 (hash-length-16 (u64+ (u64+ (hash-length-16 vf wf) (u64* (shift-mix y) +k1+)) z)
                                 (u64+ (hash-length-16 vs ws) x)))))))))

(declaim (ftype (function (octet-vector uint64 uint64
                           &key (:start vector-index) (:end vector-index))
                          (values uint64 &optional))
                city-hash-64-with-seeds))

(defun city-hash-64-with-seeds (octets seed0 seed1 &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index
START to index END together with seeds SEED0 and SEED1, each of type
(UNSIGNED-BYTE 64), and returns the 64-bit hash value as an (UNSIGNED-BYTE 64).
START defaults to zero, while END defaults to the length of OCTETS."
  (declare (type octet-vector octets)
           (type uint64 seed0 seed1)
           (type vector-index start end))
  (hash-length-16 (u64- (city-hash-64 octets :start start :end end) seed0) seed1))

(declaim (ftype (function (octet-vector uint64 &key (:start vector-index) (:end vector-index))
                          (values uint64 &optional))
                city-hash-64-with-seed))

(defun city-hash-64-with-seed (octets seed &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index
START to index END together with SEED of type (UNSIGNED-BYTE 64), and returns
the 64-bit hash value as an (UNSIGNED-BYTE 64).  START defaults to zero, while
END defaults to the length of OCTETS."
  (declare (type octet-vector octets)
           (type uint64 seed)
           (type vector-index start end))
  (city-hash-64-with-seeds octets +k2+ seed :start start :end end))

(declaim (ftype (function (octet-vector uint64 uint64 vector-index vector-index)
                          (values uint64 uint64 &optional))
                city-murmur)
         (inline city-murmur))

(defun city-murmur (octets x y start length)
  "Returns a 128-bit hash code.  Based on City and Murmur128."
  (declare (type octet-vector octets)
           (type uint64 x y)
           (type vector-index start length))
  (let ((a x)
        (b y)
        (c 0)
        (d 0)
        (len (- length 16)))
    (declare (type uint64 a b c d))
    (if (<= len 0)                      ; length <= 16
        (progn
          (setf a (u64* (shift-mix (u64* a +k1+)) +k1+))
          (setf c (u64+ (u64* b +k1+) (hash-length-0-to-16 octets start length)))
          (setf d (shift-mix (u64+ a (if (>= length 8) (ub64ref/le octets start) c)))))
        (progn                          ; length > 16
          (setf c (hash-length-16 (u64+ (ub64ref/le octets (+ start length -8)) +k1+) a))
          (setf d (hash-length-16 (u64+ b length)
                                  (u64+ c (ub64ref/le octets (+ start length -16)))))
          (incf64 a d)
          (loop for index of-type vector-index upfrom start by 16
                do (setf a (logxor a (u64* (shift-mix (u64* (ub64ref/le octets index) +k1+))
                                           +k1+)))
                   (setf a (u64* a +k1+))
                   (setf b (logxor b a))
                   (setf c (logxor c (u64* (shift-mix (u64* (ub64ref/le octets (+ index 8)) +k1+))
                                           +k1+)))
                   (setf c (u64* c +k1+))
                   (setf d (logxor d c))
                   (decf len 16)
                while (> len 0))))
    (setf a (hash-length-16 a c))
    (setf b (hash-length-16 d b))
    (values (logxor a b) (hash-length-16 b a))))

(declaim (ftype (function (octet-vector uint64 uint64
                           &key (:start vector-index) (:end vector-index))
                          (values uint64 uint64 &optional))
                city-hash-128-with-seed))

(defun city-hash-128-with-seed (octets x y &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index
START to index END together with seeds X and Y, each of type (UNSIGNED-BYTE 64),
and returns the 128-bit hash value as two values of type (UNSIGNED-BYTE 64).
START defaults to zero, while END defaults to the length of OCTETS."
  (declare (type octet-vector octets)
           (type uint64 x y)
           (type vector-index start end))
  (let ((length (- end start)))
    (declare (type vector-index length))
    (if (< length 128)
        (city-murmur octets x y start length)
        ;; We expect length >= 128 to be the common case.  Keep 56 bytes of state: x, y, z, vf, vs,
        ;; wf, ws.
        (let* ((z (u64* length +k1+))
               (vf (u64+ (u64* (rotate-right-64 (logxor y +k1+) 49) +k1+)
                         (ub64ref/le octets start)))
               (vs (u64+ (u64* (rotate-right-64 vf 42) +k1+) (ub64ref/le octets (+ start 8))))
               (wf (u64+ (u64* (rotate-right-64 (u64+ y z) 35) +k1+) x))
               (ws (u64* (rotate-right-64 (u64+ x (ub64ref/le octets (+ start 88))) 53) +k1+))
               (index start))
          (declare (type vector-index index))
          ;; This is the same inner loop as CityHash64(), manually unrolled.
          (loop do (setf x (u64* (rotate-right-64
                                  (u64+ (u64+ (u64+ x y) vf) (ub64ref/le octets (+ index 8)))
                                  37)
                                 +k1+))
                   (setf y (u64* (rotate-right-64
                                  (u64+ (u64+ y vs) (ub64ref/le octets (+ index 48)))
                                  42)
                                 +k1+))
                   (setf x (logxor x ws))
                   (incf64 y (u64+ vf (ub64ref/le octets (+ index 40))))
                   (setf z (u64* (rotate-right-64 (u64+ z wf) 33) +k1+))
                   (setf (values vf vs)
                         (weak-hash-length-32-with-seeds octets index (u64* vs +k1+) (u64+ x wf)))
                   (setf (values wf ws)
                         (weak-hash-length-32-with-seeds octets (+ index 32) (u64+ z ws)
                                                         (u64+ y
                                                               (ub64ref/le octets (+ index 16)))))
                   (rotatef z x)
                   (incf index 64)
                   (setf x (u64* (rotate-right-64
                                  (u64+ (u64+ (u64+ x y) vf) (ub64ref/le octets (+ index 8)))
                                  37)
                                 +k1+))
                   (setf y (u64* (rotate-right-64
                                  (u64+ (u64+ y vs) (ub64ref/le octets (+ index 48)))
                                  42)
                                 +k1+))
                   (setf x (logxor x ws))
                   (incf64 y (u64+ vf (ub64ref/le octets (+ index 40))))
                   (setf z (u64* (rotate-right-64 (u64+ z wf) 33) +k1+))
                   (setf (values vf vs)
                         (weak-hash-length-32-with-seeds octets index (u64* vs +k1+) (u64+ x wf)))
                   (setf (values wf ws)
                         (weak-hash-length-32-with-seeds octets (+ index 32) (u64+ z ws)
                                                         (u64+ y
                                                               (ub64ref/le octets (+ index 16)))))
                   (rotatef z x)
                   (incf index 64)
                   (decf length 128)
                while (>= length 128))
          (incf64 x (u64* (rotate-right-64 (u64+ vf z) 49) +k0+))
          (incf64 z (u64* (rotate-right-64 wf 37) +k0+))
          ;; If 0 < length < 128, hash up to 4 chunks of 32 bytes each from the end of octets.
          (loop with tail-done = 0
                while (< tail-done length)
                do (incf tail-done 32)
                   (setf y (u64+ (u64* (rotate-right-64 (u64+ y x) 42) +k0+) vs))
                   (incf64 wf (ub64ref/le octets (- (+ index length 16) tail-done)))
                   (setf x (u64+ (u64* x +k0+) wf))
                   (incf64 z (u64+ ws (ub64ref/le octets (- (+ index length) tail-done))))
                   (incf64 ws vf)
                   (setf (values vf vs)
                         (weak-hash-length-32-with-seeds octets
                                                         (- (+ index length) tail-done)
                                                         (u64+ vf z)
                                                         vs)))
          ;; At this point our 56 bytes of state should contain more than enough information for a
          ;; strong 128-bit hash.  We use two different 56-byte-to-8-byte hashes to get a 16-byte
          ;; final result.
          (setf x (hash-length-16 x vf))
          (setf y (hash-length-16 (u64+ y z) wf))
          (values (u64+ (hash-length-16 (u64+ x vs) ws) y)
                  (hash-length-16 (u64+ x ws) (u64+ y vs)))))))

(declaim (ftype (function (octet-vector &key (:start vector-index) (:end vector-index))
                          (values uint64 uint64 &optional))
                city-hash-128))

(defun city-hash-128 (octets &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index
START to index END and returns the 128-bit hash value as two values of
type (UNSIGNED-BYTE 64).  START defaults to zero, while END defaults to the
length of OCTETS."
  (declare (type octet-vector octets)
           (type vector-index start end))
  (let ((length (- end start)))
    (multiple-value-bind (hf hs)
        (cond ((>= length 16)
               (city-hash-128-with-seed octets
                                        (logxor (ub64ref/le octets start) +k3+)
                                        (ub64ref/le octets (+ start 8))
                                        :start (+ start 16)
                                        :end end))
              ((>= length 8)
               (city-hash-128-with-seed +empty-octet-vector+
                                        (logxor (ub64ref/le octets start) (u64* length +k0+))
                                        (logxor (ub64ref/le octets (- end 8)) +k1+)
                                        :start 0
                                        :end 0))
              (t
               (city-hash-128-with-seed octets +k0+ +k1+ :start start :end end)))
      (values hf hs))))
