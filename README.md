# city-hash

A Common Lisp implementation of Google's CityHash family of hash functions.

For more information see the [CityHash web site](https://github.com/google/cityhash).

## The city-hash API

#### city-hash-32 octets &key (start 0) (end (length octets))

```
Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8) from index START
to index END and returns the 32-bit hash value as an (UNSIGNED-BYTE 32).  START
defaults to zero, while END defaults to the length of OCTETS.
```

#### city-hash-64 octets &key (start 0) (end (length octets)

```
Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8) from index START
to index END and returns the 64-bit hash value as an (UNSIGNED-BYTE 64).  START
defaults to zero, while END defaults to the length of OCTETS.
```

#### city-hash-64-with-seeds octets seed0 seed1 &key (start 0) (end (length octets))

```
Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index START
to index END together with seeds SEED0 and SEED1, each of type (UNSIGNED-BYTE 64),
and returns the 64-bit hash value as an (UNSIGNED-BYTE 64).  START defaults
to zero, while END defaults to the length of OCTETS.
```

#### city-hash-64-with-seed octets seed &key (start 0) (end (length octets))

```
Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index START
to index END together with SEED of type (UNSIGNED-BYTE 64), and returns the
64-bit hash value as an (UNSIGNED-BYTE 64).  START defaults to zero, while END
defaults to the length of OCTETS.
```

#### city-hash-128-with-seed octets x y &key (start 0) (end (length octets))

```
Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index START
to index END together with seeds X and Y, each of type (UNSIGNED-BYTE 64), and
returns the 128-bit hash value as two values of type (UNSIGNED-BYTE 64).  START
defaults to zero, while END defaults to the length of OCTETS.
```

#### city-hash-128 octets &key (start 0) (end (length octets))

```
Hashes the contents of OCTETS, a vector of (UNSIGNED-BYTE 8), from index START
to index END and returns the 128-bit hash value as two values of type
(UNSIGNED-BYTE 64).  START defaults to zero, while END defaults to the length
of OCTETS.
```
