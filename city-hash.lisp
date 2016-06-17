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

;;;; CityHash v1.1.1, by Geoff Pike and Jyrki Alakuijala.
;;;; Translated into Common Lisp by Robert Brown (robert.brown@gmail.com).

(in-package #:city-hash)

(defmacro mod-2^32 (x) `(logand ,x #xffffffff))

(defmacro u32+ (x y) `(mod-2^32 (+ ,x ,y)))
(defmacro u32* (x y) `(mod-2^32 (* ,x ,y)))

(define-modify-macro incf32 (x) u32+ "Increment, modulo 2^32, PLACE by X.")
(define-modify-macro mulf32 (x) u32* "Multiply, modulo 2^32, PLACE by X.")

(define-modify-macro logxorf (x) logxor "Logically exclusive or PLACE by X.")
(define-modify-macro rotatef-right-32 (x) rotate-right-32
  "Rotate 32-bit PLACE right by X bit positions.")

(declaim (inline rotate-right-32))

(defun rotate-right-32 (x shift)
  (declare (type uint32 x)
           (type (integer 11 19) shift))
  #+sbcl
  (sb-rotate-byte:rotate-byte (- shift) (byte 32 0) x)
  #-sbcl
  (logior (ash x (- shift)) (mod-2^32 (ash x (- 32 shift)))))

;; Magic numbers for 32-bit hashing.  Copied from Murmur3.
(defconst +c1+ #xcc9e2d51)
(defconst +c2+ #x1b873593)
(defconst +n+ #xe6546b64)

(declaim (inline fmix))

(defun fmix (h)
  "A 32-bit to 32-bit integer hash copied from Murmur3."
  (declare (type uint32 h))
  (logxorf h (ash h -16))
  (mulf32 h #x85ebca6b)
  (logxorf h (ash h -13))
  (mulf32 h #xc2b2ae35)
  (logxor h (ash h -16)))

(declaim (inline mur))

(defun mur (a h)
  "Helper from Murmur3 for combining two 32-bit values."
  (declare (type uint32 a h))
  (mulf32 a +c1+)
  (rotatef-right-32 a 17)
  (mulf32 a +c2+)
  (logxorf h a)
  (rotatef-right-32 h 19)
  (mod-2^32 (+ (* h 5) +n+)))

(defun hash32-length-0-to-4 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index)
           (type (integer 0 4) length))
  (let ((b 0)
        (c 9))
    (declare (type uint32 b c))
    (dotimes (offset length)
      (let ((v (aref octets (+ index offset))))
        (setf v (if (logbitp 7 v) (- v 256) v))
        (mulf32 b +c1+)
        (incf32 b v)
        (logxorf c b)))
    (fmix (mur b (mur length c)))))

(defun hash32-length-5-to-12 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index)
           (type (integer 5 12) length))
  (let* ((a length)
         (b (* 5 length))
         (c 9)
         (d b))
    (declare (type uint32 a b c d))
    (incf32 a (ub32ref/le octets index))
    (incf32 b (ub32ref/le octets (+ index length -4)))
    (incf32 c (ub32ref/le octets (+ index (logand (ash length -1) 4))))
    (fmix (mur c (mur b (mur a d))))))

(defun hash32-length-13-to-24 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index)
           (type (integer 13 24) length))
  (let ((a (ub32ref/le octets (+ index -4 (ash length -1))))
        (b (ub32ref/le octets (+ index 4)))
        (c (ub32ref/le octets (+ index length -8)))
        (d (ub32ref/le octets (+ index (ash length -1))))
        (e (ub32ref/le octets index))
        (f (ub32ref/le octets (+ index length -4)))
        (h length))
    (declare (type uint32 a b c d e f h))
    (fmix (mur f (mur e (mur d (mur c (mur b (mur a h)))))))))

(declaim (ftype (function (octet-vector &key (:start vector-index) (:end vector-index))
                          (values uint32 &optional))
                city-hash-32))

(defun city-hash-32 (octets &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8) from index
START to index END and returns the 32-bit hash value as an (UNSIGNED-BYTE 32).
START defaults to zero, while END defaults to the length of OCTETS."
  (declare (type octet-vector octets)
           (type vector-index start end))
  (let ((length (- end start)))
    (declare (type vector-index length))
    (if (<= length 24)
        (if (<= length 12)
            (if (<= length 4)
                (hash32-length-0-to-4 octets start length)
                (hash32-length-5-to-12 octets start length))
            (hash32-length-13-to-24 octets start length))
        ;; length > 24
        (let* ((h length)
               (g (u32* +c1+ length))
               (f g)
               (a0 (u32* (rotate-right-32 (u32* (ub32ref/le octets (- end 4)) +c1+) 17) +c2+))
               (a1 (u32* (rotate-right-32 (u32* (ub32ref/le octets (- end 8)) +c1+) 17) +c2+))
               (a2 (u32* (rotate-right-32 (u32* (ub32ref/le octets (- end 16)) +c1+) 17) +c2+))
               (a3 (u32* (rotate-right-32 (u32* (ub32ref/le octets (- end 12)) +c1+) 17) +c2+))
               (a4 (u32* (rotate-right-32 (u32* (ub32ref/le octets (- end 20)) +c1+) 17) +c2+)))
          (declare (type uint32 h g f a0 a1 a2 a3 a4))
          (logxorf h a0)
          (rotatef-right-32 h 19)
          (setf h (mod-2^32 (+ (* h 5) +n+)))
          (logxorf h a2)
          (rotatef-right-32 h 19)
          (setf h (mod-2^32 (+ (* h 5) +n+)))
          (logxorf g a1)
          (rotatef-right-32 g 19)
          (setf g (mod-2^32 (+ (* g 5) +n+)))
          (logxorf g a3)
          (rotatef-right-32 g 19)
          (setf g (mod-2^32 (+ (* g 5) +n+)))
          (incf32 f a4)
          (rotatef-right-32 f 19)
          (setf f (mod-2^32 (+ (* f 5) +n+)))
          (loop repeat (floor (1- length) 20) do
            (let ((a0 (u32* (rotate-right-32 (u32* (ub32ref/le octets start) +c1+) 17) +c2+))
                  (a1 (ub32ref/le octets (+ start 4)))
                  (a2 (u32* (rotate-right-32 (u32* (ub32ref/le octets (+ start 8)) +c1+) 17) +c2+))
                  (a3 (u32* (rotate-right-32 (u32* (ub32ref/le octets (+ start 12)) +c1+) 17) +c2+))
                  (a4 (ub32ref/le octets (+ start 16))))
              (declare (type uint32 h g f a0 a1 a2 a3 a4))
              (logxorf h a0)
              (rotatef-right-32 h 18)
              (setf h (mod-2^32 (+ (* h 5) +n+)))
              (incf32 f a1)
              (rotatef-right-32 f 19)
              (mulf32 f +c1+)
              (incf32 g a2)
              (rotatef-right-32 g 18)
              (setf g (mod-2^32 (+ (* g 5) +n+)))
              (logxorf h (u32+ a3 a1))
              (rotatef-right-32 h 19)
              (setf h (mod-2^32 (+ (* h 5) +n+)))
              (logxorf g a4)
              (setf g (u32* (swap-bytes-32 g) 5))
              (incf32 h (u32* a4 5))
              (setf h (swap-bytes-32 h))
              (incf32 f a0)
              (rotatef f h)
              (rotatef f g)
              (incf start 20)))
          (rotatef-right-32 g 11)
          (mulf32 g +c1+)
          (rotatef-right-32 g 17)
          (mulf32 g +c1+)
          (rotatef-right-32 f 11)
          (mulf32 f +c1+)
          (rotatef-right-32 f 17)
          (mulf32 f +c1+)
          (setf h (rotate-right-32 (u32+ h g) 19))
          (setf h (mod-2^32 (+ (* h 5) +n+)))
          (setf h (u32* (rotate-right-32 h 17) +c1+))
          (setf h (rotate-right-32 (u32+ h f) 19))
          (setf h (mod-2^32 (+ (* h 5) +n+)))
          (u32* (rotate-right-32 h 17) +c1+)))))

;; Random primes between 2^63 and 2^64.
(defconst +k+ #x9ddfea08eb382d69)
(defconst +k0+ #xc3a5c85c97cb3127)
(defconst +k1+ #xb492b66fbe98f273)
(defconst +k2+ #x9ae16a3b2f90404f)

(defconst +empty-octet-vector+ (make-octet-vector 0))

(defmacro mod-2^64 (x) `(logand ,x #xffffffffffffffff))

(defmacro u64+ (x y) `(mod-2^64 (+ ,x ,y)))
(defmacro u64- (x y) `(mod-2^64 (- ,x ,y)))
(defmacro u64* (x y) `(mod-2^64 (* ,x ,y)))

(define-modify-macro incf64 (x) u64+ "Increment, modulo 2^64, PLACE by X.")
(define-modify-macro mulf64 (x) u64* "Multiply, modulo 2^64, PLACE by X.")

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

(declaim (inline hash64-length-16))

(defun hash64-length-16 (u v mul)
  (declare (type uint64 u v mul))
  ;; Murmur-inspired hashing.
  (let* ((a (shift-mix (u64* (logxor u v) mul)))
         (b (shift-mix (u64* (logxor v a) mul))))
    (u64* b mul)))

(defun hash64-length-0-to-16 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index length))
  (cond ((>= length 8)
         (let* ((mul (u64+ +k2+ (u64* length 2)))
                (a (u64+ (ub64ref/le octets index) +k2+))
                (b (ub64ref/le octets (+ index length -8)))
                (c (u64+ (u64* (rotate-right-64 b 37) mul) a))
                (d (u64* (u64+ (rotate-right-64 a 25) b) mul)))
           (hash64-length-16 c d mul)))
        ((>= length 4)
         (let ((mul (u64+ +k2+ (u64* length 2)))
               (a (ub32ref/le octets index)))
           (hash64-length-16 (u64+ length (ash a 3))
                             (ub32ref/le octets (+ index length -4))
                             mul)))
        ((plusp length)
         (let* ((a (aref octets index))
                (b (aref octets (+ index (ash length -1))))
                (c (aref octets (+ index length -1)))
                (y (+ a (ash b 8)))
                (z (+ length (ash c 2))))
           (u64* (shift-mix (logxor (u64* y +k2+) (u64* z +k0+))) +k2+)))
        (t +k2+)))

(defun hash64-length-17-to-32 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index length))
  (let* ((mul (u64+ +k2+ (u64* length 2)))
         (a (u64* (ub64ref/le octets index) +k1+))
         (b (ub64ref/le octets (+ index 8)))
         (c (u64* (ub64ref/le octets (+ index length -8)) mul))
         (d (u64* (ub64ref/le octets (+ index length -16)) +k2+)))
    (hash64-length-16 (u64+ (u64+ (rotate-right-64 (u64+ a b) 43) (rotate-right-64 c 30)) d)
                      (u64+ (u64+ a (rotate-right-64 (u64+ b +k2+) 18)) c)
                      mul)))

(declaim (inline weak-hash64-length-32-with-seeds))

(defun weak-hash64-length-32-with-seeds (octets index a b)
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

(defun hash64-length-33-to-64 (octets index length)
  (declare (type octet-vector octets)
           (type vector-index index length))
  (let* ((mul (u64+ +k2+ (u64* length 2)))
         (a (u64* (ub64ref/le octets index) +k2+))
         (b (ub64ref/le octets (+ index 8)))
         (c (ub64ref/le octets (+ index length -24)))
         (d (ub64ref/le octets (+ index length -32)))
         (e (u64* (ub64ref/le octets (+ index 16)) +k2+))
         (f (u64* (ub64ref/le octets (+ index 24)) 9))
         (g (ub64ref/le octets (+ index length -8)))
         (h (u64* (ub64ref/le octets (+ index length -16)) mul))
         (u (u64+ (rotate-right-64 (u64+ a g) 43)
                  (u64* (u64+ (rotate-right-64 b 30) c) 9)))
         (v (u64+ (u64+ (logxor (u64+ a g) d) f) 1))
         (w (u64+ (swap-bytes-64 (u64* (u64+ u v) mul)) h))
         (x (u64+ (rotate-right-64 (u64+ e f) 42) c))
         (y (u64* (u64+ (swap-bytes-64 (u64* (u64+ v w) mul)) g) mul))
         (z (u64+ (u64+ e f) c)))
    (setf a (u64+ (swap-bytes-64 (u64+ (u64* (u64+ x z) mul) y)) b))
    (setf b (u64* (shift-mix (u64+ (u64+ (u64* (u64+ z a) mul) d) h)) mul))
    (u64+ b x)))

(declaim (ftype (function (octet-vector &key (:start vector-index) (:end vector-index))
                          (values uint64 &optional))
                city-hash-64))

(defun city-hash-64 (octets &key (start 0) (end (length octets)))
  "Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8) from index
START to index END and returns the 64-bit hash value as an (UNSIGNED-BYTE 64).
START defaults to zero, while END defaults to the length of OCTETS."
  (declare (type octet-vector octets)
           (type vector-index start end))
  (let ((length (- end start)))
    (declare (type vector-index length))
    (cond ((<= length 32)
           (if (<= length 16)
               (hash64-length-0-to-16 octets start length)
               (hash64-length-17-to-32 octets start length)))
          ((<= length 64)
           (hash64-length-33-to-64 octets start length))
          (t
           ;; For strings over 64 bytes we hash the end first, and then as we loop we keep 56 bytes
           ;; of state: x, y, z, vf, vs, wf, and ws.
           (let ((x (ub64ref/le octets (- end 40)))
                 (y (u64+ (ub64ref/le octets (- end 16)) (ub64ref/le octets (- end 56))))
                 (z (hash64-length-16 (u64+ (ub64ref/le octets (- end 48)) length)
                                      (ub64ref/le octets (- end 24))
                                      +k+)))
             (multiple-value-bind (vf vs)
                 (weak-hash64-length-32-with-seeds octets (- end 64) length z)
               (multiple-value-bind (wf ws)
                   (weak-hash64-length-32-with-seeds octets (- end 32) (u64+ y +k1+) x)
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
                                (weak-hash64-length-32-with-seeds
                                 octets index (u64* vs +k1+) (u64+ x wf)))
                          (setf (values wf ws)
                                (weak-hash64-length-32-with-seeds
                                 octets (+ index 32) (u64+ z ws)
                                 (u64+ y (ub64ref/le octets (+ index 16)))))
                          (rotatef z x)
                          (decf length 64)
                       while (not (zerop length)))
                 (hash64-length-16
                  (u64+ (u64+ (hash64-length-16 vf wf +k+) (u64* (shift-mix y) +k1+)) z)
                  (u64+ (hash64-length-16 vs ws +k+) x)
                  +k+))))))))

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
  (hash64-length-16 (u64- (city-hash-64 octets :start start :end end) seed0) seed1 +k+))

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
          (setf c (u64+ (u64* b +k1+) (hash64-length-0-to-16 octets start length)))
          (setf d (shift-mix (u64+ a (if (>= length 8) (ub64ref/le octets start) c)))))
        (progn                          ; length > 16
          (setf c (hash64-length-16 (u64+ (ub64ref/le octets (+ start length -8)) +k1+) a +k+))
          (setf d (hash64-length-16 (u64+ b length)
                                    (u64+ c (ub64ref/le octets (+ start length -16)))
                                    +k+))
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
    (setf a (hash64-length-16 a c +k+))
    (setf b (hash64-length-16 d b +k+))
    (values (logxor a b) (hash64-length-16 b a +k+))))

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
                         (weak-hash64-length-32-with-seeds octets index (u64* vs +k1+) (u64+ x wf)))
                   (setf (values wf ws)
                         (weak-hash64-length-32-with-seeds octets (+ index 32) (u64+ z ws)
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
                         (weak-hash64-length-32-with-seeds octets index (u64* vs +k1+) (u64+ x wf)))
                   (setf (values wf ws)
                         (weak-hash64-length-32-with-seeds octets (+ index 32) (u64+ z ws)
                                                           (u64+ y
                                                                 (ub64ref/le octets (+ index 16)))))
                   (rotatef z x)
                   (incf index 64)
                   (decf length 128)
                while (>= length 128))
          (incf64 x (u64* (rotate-right-64 (u64+ vf z) 49) +k0+))
          (setf y (u64+ (u64* y +k0+) (rotate-right-64 ws 37)))
          (setf z (u64+ (u64* z +k0+) (rotate-right-64 wf 27)))
          (mulf64 wf 9)
          (mulf64 vf +k0+)
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
                         (weak-hash64-length-32-with-seeds octets
                                                           (- (+ index length) tail-done)
                                                           (u64+ vf z)
                                                           vs))
                   (mulf64 vf +k0+))
          ;; At this point our 56 bytes of state should contain more than enough information for a
          ;; strong 128-bit hash.  We use two different 56-byte-to-8-byte hashes to get a 16-byte
          ;; final result.
          (setf x (hash64-length-16 x vf +k+))
          (setf y (hash64-length-16 (u64+ y z) wf +k+))
          (values (u64+ (hash64-length-16 (u64+ x vs) ws +k+) y)
                  (hash64-length-16 (u64+ x ws) (u64+ y vs) +k+))))))

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
    (if (>= length 16)
        (city-hash-128-with-seed octets
                                 (ub64ref/le octets start)
                                 (u64+ (ub64ref/le octets (+ start 8)) +k0+)
                                 :start (+ start 16)
                                 :end end)
        (city-hash-128-with-seed octets +k0+ +k1+ :start start :end end))))
