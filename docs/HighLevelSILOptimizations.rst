:orphan:

High-Level Optimizations in SIL
===============================

.. contents::

Abstract
--------

This document describes the high-level abstraction of built-in Swift
data structures in SIL that is used by the optimizer. You need to read
this document if you wish to understand the early stages of the Swift
optimizer or if you are working on one of the containers in the 
standard library.


Why do we need high-level optimizations?
-----------------------------------------

Swift containers are implemented in the Swift standard library in Swift code.
Traditional compiler optimizations can remove some of the redundancy that is
found in high-level code, but not all of it. Without knowledge of the Swift
language the optimizer can't perform high-level optimizations on the built-in
containers. For example::

  Dict["a"] = 1
  Dict["a"] = 2

Any Swift developer could identify the redundancy in the code sample above.
Storing two values into the same key in the dictionary is inefficient.
However, optimizing compilers are unaware of the special semantics that the
Swift dictionary has and can't perform this optimization. Traditional
compilers would start optimizing this code by inlining the subscript 
function call and try to analyze the sequence of load/store instructions.
This approach is not very effective because the compiler has to be very
conservative when optimizing general code with pointers. 

On the other hand, compilers for high-level languages usually have special
bytecode instructions that allow them to perform high-level optimizations.
However, unlike high-level languages such as JavaScript or Python, Swift
containers are implemented in Swift itself. Moreover, it is beneficial to
be able to inline code from the container into the user program and optimize
them together, especially for code that uses Generics. 

In order to perform both high-level optimizations, that are common in
high-level languages, and low-level optimizations we annotate parts of the
standard library and describe the semantics of a domain-specific high-level
Swift bytecode. 

Annotation of code in the standard library
------------------------------------------

We use the ``@semantics`` attribute to annotate code in the standard library.
These annotations can be used by the high-level SIL optimizer to perform
domain-specific optimizations.

This is an example of the ``@semantics`` attribute::

  @public @semantics("array.count")
  func getCount() -> Int {
    return _buffer.count
   }

In this example we annotate a member of the Swift array struct with the tag
``array.count``. This tag informs the optimizer that this method reads the
size of the array.


The ``@semantics`` attribute allows us to define "builtin" SIL-level
operations implemented in Swift code. In SIL code they are encoded as
apply instructions, but the optimizer can operate on them as atomic
instructions. The semantic annotations don't necessarily need to be on
public APIs. For example, the Array subscript operator may contain two
bytecode instructions. One for checking the bounds and another one for
accessing the elements. With this abstraction the optimizer can remove
the ``checkBounds`` instruction and keep the getElement instruction::
 
  @public subscript(index: Int) -> Element {
     get {
      checkBounds(index)
      return getElement(index)
     }

  @semantics("array.check_bounds") func checkBounds(index: Int) {
    ...
  }
	
  @semantics("array.get_element") func getElement(index: Int) -> Element {
    return _buffer[index]
  }


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

The following semantic tags describe Array operations. The operations
are first described in terms of the Array "state". Relations between the
operations are formally defined below.

We consider the array state to consist of a set of disjoint elements
and a storage descriptor that encapsulates nonelement data such as the
element count and capacity. Operations that semantically write state
are always *control dependent*. A control dependent operation is one
that may only be executed on the control flow paths in which the
operation originally appeared, ignoring potential program
exits. Generally, operations that only read state are not control
dependent. One exception is ``check_bounds`` which is readonly but
control dependent. Some operation are *guarded* by others. A guarded
operation may execute on any path as long as the guard/operation
sequence is preserved. All semantic operations are idempotent if they
call the same function with the same argument values, only with the
exception of ``mutate_unknown``.

array.get_element(index: Int) -> Element

   Read an element from the array at the specified index. No other
   elements are read. The storage descriptor is not read. No state is
   written. This operation is not control dependent, but may be
   guarded by ``check_bounds``. Any ``check_bounds`` may act as a
   guard, regardless of the index being checked [#f1]_.

array.set_element(index: Int, e: Element)
   
  Write an element into the array at the specified index. No state is
  read. No other elements are written. The storage descriptor is not
  written. This operation is control dependent and may be guarded by
  ``check_bounds`` and ``make_mutable`` or ``mutate_unknown``.

array.check_bounds(index: Int)

  Read the array count from the storage descriptor. Execute a ``trap``
  if ``index < 0 || index >= count``. No elements are read. No state
  is written. Despite being read only, this operation is control
  dependent.

array.get_count() -> Int

  Read the array count from the storage descriptor. No elements are
  read. No state is written. This is neither guarded nor control dependent.

array.get_capacity() -> Int

  Read the array capacity from the storage descriptor. The semantics
  are identical to ``get_count`` except for the meaning of the return value.

array.make_mutable()

  This operation guards mutating operations that don't already imply
  ``make_mutable`` semantics. (Currently, the only guarded operation
  is ``set_element``.) ``make_mutable`` may create a copy of the array
  storage; however, semantically it neither reads nor writes the array
  state. It does not write state simply because the copy's state is
  identical to the original. It does not read state because no other
  Array operations can undo mutability--only code that retains a
  reference to the Array can do that. ``make_mutable`` does
  effectively need to be guarded by any SIL operation that may retain
  the array. Because ``make_mutable`` semantically does not read the
  array state, is idempotent, and has no control dependence, it can be
  executed safely on any array at any point. i.e. the optimizer can
  freely insert calls to make_mutable.

array.mutate_unknown()

  This operation may mutate the array in any way, so it semantically
  writes to the entire array state and is naturally control
  dependent. ``mutate_unknown`` also implies the guarding semantics of
  ``make_mutable``. It is not itself guarded by ``make_mutable`` and
  may act as a guard to other mutating operations, such as
  ``set_element``. Combining semantics allows the flexbility in how
  the array copy is implemented in conjunction with implementing
  mutating functionality. This may be more efficient than cleanly
  isolating the copy and mutation code.

To complete the semantics understood by the optimizer, we define these relations:

interferes-with
  
  Given idempotent ``OpA``, the the sequence "``OpA, OpB, OpA``" is
  semantically equivalent to the sequence "``OpA, OpB``" *iff* ``OpB``
  does not interfere with ``OpA``.

guards

  If ``OpA`` guards ``OpB``, then the sequence of operations
  ``OpA,OpB`` must be preserved on any control flow path on which the
  sequence originally appears.

An operation can only interfere-with or guard another if they may operate on the same Array.

============== =============== ==========================================
semantic op    relation        semantic ops
============== =============== ==========================================
make_mutable   guards          set_element
check_bounds   guards          get_element, set_element
set_element(i) interferes-with get_element(i)
mutate_unknown itereferes-with get_element, set_element, check_bounds,
                               get_count, get_capacity
============== =============== ==========================================

.. [#f1] Any check_bounds(N) may act as a guard for
         ``get/set_element(i)`` as long as it can be shown that ``N >=
         i``.

In addition to preserving these semantics, the optimizer must conservatively handle any unknown access to the array object.

String
~~~~~~
TBD.

Dictionary
~~~~~~~~~~
TBD.

Other semantic attributes
~~~~~~~~~~~~~~~~~~~~~~~~~~~

pure

  Marking a function with @semantics("pure") indicates that the function computes its result strictly based on its arguments.
  The function may dereference memory based on pointer arguments but it may not mutate any state.
  Pure functions do not write through any pointer arguments and never change any state visible to callers.


