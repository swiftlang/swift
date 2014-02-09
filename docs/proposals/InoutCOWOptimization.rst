:orphan:
   
================================================
 Copy-On-Write Optimization of ``inout`` Values
================================================

:Authors: Dave Abrahams, Joe Groff
          
:Summary: Our writeback model interacts with Copy-On-Write (COW) to
          cause some surprising ineffiencies, such as O(N) performance
          for ``x[0][0] = 1``. We propose a modified COW optimization
          that recovers O(1) performance for these cases and supports
          the efficient use of slices in algorithm implementation.

Whence the Problem?
===================

The problem is caused as follows:

* COW depends on the programmer being able to mediate all writes (so
  she can copy if necessary)
  
* Writes to container elements and slices are mediated through
  subscript setters, so in ::

    x[0].mutate()

  we “``subscript get:``” ``x[0]`` into a temporary, mutate the
  temporary, and “``subscript set:``” it back into ``x[0]``.

* When the element itself is a COW type, that temporary implies a
  retain count of at least 2 on the element's buffer.

* Therefore, mutating such an element causes an expensive copy, *even
  when the element's buffer isn't otherwise shared*.

Naturally, this problem generalizes to any COW value backed by a
getter/setter pair, such as a computed or resilient ``String``
property::

  anObject.title.append('.') // O(N)

Interaction With Slices
=======================

Consider the classic divide-and-conquer algorithm QuickSort, which
could be written as follows:

.. parsed-literal::

  protocol Sliceable {
    ...
    @mutating
    func quickSort(compare: (StreamType.Element, StreamType.Element)->Bool) {
      let (start,end) = (startIndex(), endIndex())
      if start != end && start.succ() != end {
        let pivot = self[start]
        let mid = partition({compare($0, pivot)})
        **self[start...mid].quickSort(compare)**
        **self[mid...end].quickSort(compare)**
      }
    }
  }

The implicit ``inout`` on the target of the recursive ``quickSort``
calls currently forces two allocations and O(N) copies in each layer
of the QuickSort implementation.  Note that this problem applies to
simple containers such as ``Int[]``, not just containers of COW
elements.

Without solving this problem, mutating algorithms must operate on
``MutableCollection``\ s and pairs of their ``Index`` types, and we
must hope the ARC optimizer is able to eliminate the additional
reference at the top-level call.  However, that does nothing for the
cases mentioned in the previous section.

Our Solution
============

We need to prevent lvalues created in an ``inout`` context from
forcing a copy-on-write.  To accomplish that:

* We give COW buffers a property ``TOP`` that can store the address of
  some reference to the buffer.
  
* When a unique reference ``r`` to a COW buffer ``b`` is copied into
  an ``inout`` lvalue, we store ``r``\ 's address in ``b.TOP``.

* When ``b`` is written into a non-``inout`` lvalue, ``b.TOP`` is
  cleared.

* A COW buffer can be modified in-place when it is uniquely referenced
  *or* when its ``TOP`` is non-null.

We believe this can be done with little user-facing change; the author
of a COW type would add an attribute to the property that storing the
buffer, and would use a slightly different check for in-place
writability.
  
Other Considered Solutions
--------------------------

Move optimization seemed like a potential solution when we first considered
this problem--given that it is already unspecified to reference a property
while an active ``inout`` reference can modify it, it seems natural to move
ownership of the value to the ``inout`` when entering writeback and move it
back to the original value when exiting writeback. We do not think it is viable
for the following reasons:

- In general, relying on optimizations to provide performance semantics is
  brittle.
- Move optimization would not be memory safe if either the original value or
  ``inout`` slice were modified to give up ownership of the original backing
  store.  Although observing a value while it has inout aliases is unspecified,
  it should remain memory-safe to do so. This should remain memory safe, albeit
  unspecified::

    var arr = [1,2,3]
    func mutate(x: inout Int[]) -> Int[] {
      x = [3...4]
      return arr[0...2]
    }
    mutate(&arr[0...2])

  Inout slices thus require strong ownership of the backing store independent
  of the original object, which must also keep strong ownership of the backing
  store.
- Move optimization requires unique referencing and would fail when there are
  multiple concurrent, non-overlapping ``inout`` slices. ``swap(&x.a, &x.b)``
  is well-defined if ``x.a`` and ``x.b`` do not access overlapping state, and
  so should ``swap(&x[0...50], &x[50...100])``.  More generally, we would like to
  use inout slicing to implement divide-and- conquer parallel algorithms, as
  in::

    async { mutate(&arr[0...50]) }
    async { mutate(&arr[50...100]) }

enter_inout and exit_inout
==========================

We propose making an optional pair of labels to computed property definitions,
``enter_inout`` and ``exit_inout``. When a property has these labels defined
and is referenced in a context that requires writeback, the ``enter_inout``
method is applied immediately after ``get``, and the ``exit_inout`` method is
applied **to the value at the time of get** immediately before ``set``.
This operation::

  mutate(&arr[a...b])

thus behaves as if by the following sequence of calls when
``enter_inout`` and ``exit_inout`` are present for the property::

  var slice = arr.subscript(a...b).get()
  var arr_orig = arr
  arr_orig.subscript(a...b).enter_inout()
  mutate(&slice)
  arr_orig.subscript(a...b).exit_inout()
  arr.subscript(a...b).set(slice)

TODO: Copying the original value ``arr`` to ``arr_orig`` creates another strong
reference to the backing store!

``enter_inout`` and ``exit_inout`` are only applied when the computed property
is used in a writeback context, such as when used as an ``inout`` parameter or
when a ``mutating`` method or property of the value is accessed. In cases where
the property is simply loaded or stored to, such as when reading or assigning
the property, they are not applied.

``enter_inout`` and ``exit_inout`` must appear together. They are
non-\ ``@mutating`` by default.

Using enter_inout and exit_inout to Optimize Slice Mutation
===========================================================

``enter_inout`` and ``exit_inout`` expose enough mechanism for a container
author to maintain an inout reference count for the container's
backing store object. For example::

  /// Backing store for a copy-on-write Array type.
  class ArrayBuffer<T> {
    /// The number of inout references to this backing store. Includes a count
    /// for the originating non-inout value.
    var inoutRefcount: Word = 1

    func _getStrongReferenceCount() -> Word {
      // Use a (currently nonexistent) builtin to access the strong reference
      // count.
      return Word(Builtin.getStrongReferenceCount(self))
    }

    func _needsToBeCopied() -> Bool {
      // Compare the strong reference count to the inout reference count.
      return _getStrongReferenceCount(self) <= inoutRefcount
    }
  }

  struct Array<T> {
    var buffer: ArrayBuffer<T>
    var start, count: Int
    
    subscript(indexes: Range<Int>) -> Array<T> {
    get:
      return slice(indexes)
    enter_inout:
      buffer.inoutRefcount++
    exit_inout:
      buffer.inoutRefcount--
    set(value: Array<T>):
      // If the slice remains in-place, we're done.
      if (value.start === start && value.count == count) {
        return
      }

      // Otherwise, we need to splice it in.
      setSliceSlow(indexes, value)
    }
  }
  

The backing store object ``ArrayBuffer`` carries the inout reference count and
uses it to decide whether it needs to be copied, and the ``enter_inout`` and
``exit_inout`` methods of the property update the reference count to allow
slices to mutate the backing store in-place for the duration of an ``inout``
reference to the slice. The setter for the slice can then short-circuit out
in the case when the mutation happens completely in-place.

Thread Safety
=============

In our current uniqueness-based COW model, thread safety falls out naturally:
if you have a singly-referenced backing store, the value itself must also be
unique, and the backing store cannot be written concurrently without there
being a race on that value. This also holds for the case of multiple inout
references. If the inout reference count matches the strong reference count,
the active inout slices cannot observe each other's referenced slices without
fundamentally racing.
