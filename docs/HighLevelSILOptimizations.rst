:orphan:

High-Level Optimizations in SIL
===============================

.. contents::

Abstract
--------

This document describes the high-level abstraction of built-in Swift
data structures in SIL that is used by the optimizer. You need to read
this document to understand the early stages of the Swift optimizer or
if you are working on some of the containers in the standard library.


Why do we need high-level optimizations?
-----------------------------------------

Swift containers are implemented in the Swift standard library in Swift code.
Traditional compiler optimizations can remove some of the redundancy that is
found in high-level code, but not all of it. Without knowledge of the Swift
language the optimizer can't perform high-level optimizations on the built-in
data types. For example::

  Dict["a"] = 1
  Dict["a"] = 2

Any Swift developer could identify the redundancy in the code sample above.
Storing two values into the same key in the dictionary is inefficient.
However, optimizing compilers are unaware of the special semantics that the
Swift dictionary has and can't perform this optimization. A traditional
compiler would start optimizing this code by inlining the subscript call
and try to analyze the sequence of load/store instructions. This approach is
not very effective because the compiler has to be very conservative when
optimizing general code with pointers.


Annotation of code in the standard library
------------------------------------------

We use the ``@semantics`` attribute to annotate code in the standard library.
These annotations can be used by the high-level SIL optimizer to perform
domain-specific optimizations.

This is an example of the ``@semantics`` attribute::

  @public @semantics("array.size")
  func getCount() -> Int {
    return _buffer.count
   }

In this example we annotate a member of the Swift array struct with the tag
``array.size``. This tag informs the optimizer that this method reads the
size of the array.


Swift optimizations
-------------------
The swift optimizer can access the information that is provided by the
``@semantics`` attribute to perform high-level optimizations. In the early
stages of the optimization pipeline the optimizer does not inline functions
with special semantics in order to allow the early high-level optimization
passes to operate on them. In the later stages of the optimization pipeline
the optimize inlines functions with special semantics to allow low-level
optimizations.


Annotated data structures in the standard library
-------------------------------------------------

This section describes the semantic tags that are assigned to data-structures
in the standard library and the axioms that the optimizer uses.

Array
~~~~~
TBD.

String
~~~~~
TBD.

Dictionary
~~~~~
TBD.


