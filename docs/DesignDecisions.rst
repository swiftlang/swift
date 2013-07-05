.. @raise litre.TestsAreMissing

Various Design Discussions in Swift
===================================

This document is intended to capture a few random design discussions and
decisions based off them.  Some of these are likely to be revisited, but we
don't want to lose the wisdom gained from previous discussions.  These are not
meant to be particularly coherent.

sort() doesn't mutate its input collection
------------------------------------------

The decision comes down to whether sort(x) (and similar algorithms) should
mutate its argument collection (C++-style) or whether it should return a new
collection whose elements are sorted, without mutating its input.  We decided to
deviate from the C++ convention and have sort return a new array of elements
that have been sorted, and to also provide a x.sort() algorithm to provide the
C++ semantics, which more obviously mutates "x".  This approach is already used
by languages like Python.

Arguments for the C++-style API:

* It is more efficient to modify a collection in place for a few reasons: 1) no
  additional memory needs to be allocated, and 2) the elements can be moved
  instead of copied.
* It is familiar to C++ users.

Arguments against the C++-style API (in no particular order):

* The functional-style interface is somewhat cleaner and maps to many other
  languages' approach well (including ObjC, python, etc).  
* We really want consistency between the different algorithms, and some of them
  will return a collection type that differs from its input element type (for
  example, map, zip/unzip, etc).  It would be weird to have code like::
  
    var a = new Int[100]
    var b = map(a, { String(x) })
    sort(b)
  
* It should be straight-forward to implement the in-place mutating algorithms
  (i.e. x.sort()) and then provide wrappers for ones that make sense to provide
  a functional-style interface (e.g. a = sort(b)) by reading the input sequence
  into a new array/vector, then applying the mutating algorithm to that
  temporary array.
* We should eventually support syntax like "somearray[a..b].sort()", so we don't
  lose generality over C++'s iterator pair style of API.
* In principle, the compiler's optimizer could be enhanced to eliminate extra
  array copies, at least in some obvious cases.
* This style of interface allows sort(x) to work on anything that is enumerable,
  allowing one to sort the entries of a dictionary by the value (for example),
  and allow sort to take *any* enumerable collection, eliminating the random
  access iterator requirement of C++.
  
  



