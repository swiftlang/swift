======================================================
Index Invalidation Rules in the Swift Standard Library
======================================================

Points to consider
==================

(1) Collections can have a value type or a reference type.

(2) Copying a value of a value type, or copying a reference has well-defined
    semantics built into the language and is not controllable by the user code.

    Consequence: value-typed collections in Swift have to use copy-on-write for
    data stored out-of-line in reference-typed buffers.

(3) We want to be able to pass/return a Collection along with its indices in a
    safe manner.

    In Swift, unlike C++, indices are not sufficient to access collection data;
    one needs an index and a collection.  Thus, merely passing a collection by
    value to a function should not invalidate indices.

General principles
==================

In C++, validity of an iterator is a property of the iterator itself, since
iterators can be dereferenced to access collection elements.

In Swift, in order to access a collection element designated by an index,
subscript operator is applied to the collection, ``C[I]``.  Thus, index is
valid or not only in context of a certain collection instance at a certain
point of program execution.  A given index can be valid for zero, one or more
than one collection instance at the same time.

An index that is valid for a certain collection designates an element of that
collection or represents a one-past-end index.

Operations that access collection elements require valid indexes (this includes
accessing using subscript operator, slicing, swapping elements, removing
elements etc.)

Using an invalid index to access elements of a collection leads to unspecified
memory-safe behavior.  (Possibilities include trapping, performing the
operation on an arbitrary element of this or any other collection etc.)
Concrete collection types can specify behavior; implementations are advised to
perform a trap.

An arbitrary index instance is not valid for an arbitrary collection instance.

The following points apply to all collections, defined in the library or by the
user:

(1) Indices obtained from a collection ``C`` via ``C.startIndex``,
    ``C.endIndex`` and other collection-specific APIs returning indices, are
    valid for ``C``.

(2) If an index ``I`` is valid for a collection ``C``, a copy of ``I`` is valid
    for ``C``.

(3) If an index ``I`` is valid for a collection ``C``, indices obtained from
    ``I`` via ``I.successor()``, ``I.predecessor()``, and other index-specific
    APIs, are valid for ``C``.
    FIXME: disallow startIndex.predecessor(), endIndex.successor()

(4) If an index ``I`` is valid for a collection ``C``, it is also valid for
    a copy of ``C``.

(5) If an index ``I`` is valid for a collection ``C``, it continues to be valid
    after a call to a non-mutating method on ``C``.

(6) Calling a non-mutating method on a collection instance does not invalidate
    any indexes.

The following points apply to all collections, but specific collections can
give different guarantees:

(1) Calling a mutating method on a collection instance invalidates all indexes
    for that collection instance.

Consequences:

- Passing a collection as an ``inout`` argument invalidates all indexes for
  that collection instance, unless the function explicitly documents stronger
  guarantees.  (The function can call mutating methods on an ``inout`` argument
  or completely replace it.)

  * ``Swift.swap()`` does not invalidate any indexes.

Additional guarantees for ``Swift.String``
==========================================

**Slicing does not invalidate indexes.**  If an index ``I`` is valid for a
``String`` ``S``, it is also valid for a ``String`` instance obtained by
slicing ``S``.

FIXME: index should be in bounds of the slice.

Additional guarantees for ``Swift.Array``, ``Swift.ContiguousArray``, ``Swift.Slice``
=====================================================================================

**Valid array indexes can be created without using Array APIs.**  Array indexes
are plain integers.  Integers that are dynamically in the range ``0..<A.count``
are valid indexes for the array or slice ``A``.  It does not matter if an index
was obtained from the collection instance, or derived from input or unrelated
data.

**Traps are guaranteed.**  Using an invalid index to designate elements of an
array or an array slice is guaranteed to perform a trap.

Additional guarantees for ``Swift.Dictionary``
==============================================

**Insertion into a Dictionary invalidates indexes only on a rehash.**  If a
``Dictionary`` has enough free buckets (guaranteed by calling an initializer or
reserving space), then inserting elements does not invalidate indexes.

Note: unlike C++'s ``std::unordered_map``, removing elements from a
``Dictionary`` invalidates indexes.

