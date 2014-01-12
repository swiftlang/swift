:orphan:
   
==============================
 Efficient Mutation of Slices
==============================

:Authors: Dave Abrahams, Joe Groff
          
We propose language and library facilities that allow slices to be
mutated "in-place" without incurring unnecessary copies.

Motivation
==========

Consider the classic divide-and-conquer algorithm QuickSort, which
could be written as follows:

.. parsed-literal::

  func quickSort<
    Seq: Sliceable
    where Seq.StreamType.Element: Comparable
  >(s: @inout Seq) {
    let (start,end) = (s.startIndex(), s.endIndex())
    if start != end && start.succ() != end {
      let pivot = s[start]
      let mid = partition(&s, { $0 < pivot })
      quicksort(**&s[start..mid]**) // Error: can't take address of rvalue
      quicksort(**&s[mid..end]**)    // Error: can't take address of rvalue
    }
  }

Today, we don't even allow that to be written, because the slices used
in the algorithm are rvalues, and therefore can't be passed as
``@inout`` parameters.  A similar and more-idiomatic formulation as a
method of ``Sliceable``, even if we had a way to express the
restriction on the ``Element`` type, would likewise be forbidden under
current rules because method targets are implicitly ``@inout``.
Here's a version that uses a comparison function:

.. parsed-literal::

  protocol Sliceable {
    @mutating
    func quickSort(compare: (StreamType.Element, StreamType.Element)->Bool) {
      let (start,end) = (startIndex(), endIndex())
      if start != end && start.succ() != end {
        let pivot = self[start]
        let mid = partition({compare($0, pivot)})
        **self[start..mid].quicksort(compare)**
        **self[mid..end].quicksort(compare)**
      }
    }
  }
  
The obvious alternative is to move the slices into temporary lvalues, as follows::
    
    @mutating
    func quickSort(compare: (StreamType.Element, StreamType.Element)->Bool) {
      let (start,end) = (startIndex(), endIndex())
      if start != end && start.succ() != end {
        let pivot = self[start]
        let mid = partition({compare($0, pivot)})
        for r in [start..mid, mid..end] {
          var **subRange = self[r]**
          subRange.quicksort(compare)
          **self[r] = subrange**
        }
      }
    }

The problem with this formulation, of course, is that sorting
``subRange`` forces it to acquire a unique copy of its elements, even
though those elements will, just a few lines later, be written back
into the range from which they were copied.  Even if it were possible
to take the address of an rvalue slice, the same arguments would
apply: since the slice is an independent value, the first mutation
would force it to be copied.  While slices must keep a reference to
the underlying buffer for memory safety and lifetime management, a
reference to an ``@inout`` slice should *not* force a copy of the
underlying buffer.

Our Solution
============

This optimization can be implemented by maintaining an **inout reference
count** for value backing store objects that tracks the number of active
``@inout`` references that are allowed to mutate the buffer without copying.
A container value and all in-place ``@inout`` slices retain strong references
to the backing store, as usual, but also bump this inout reference count while
they are allowed to mutate the backing store in-place.
The logic for deciding whether to copy a buffer prior to a
mutation changes from the current "strong refcount equals one" criterion into
"strong refcount is less than or equal to inout refcount".

Because this optimization benefits a relatively small set of slicing subscript
properties, we think it is reasonable to expose it as a manual optimization for
the library. To expose this implementation strategy to the library, we propose
to create a special pair of property operations that can be used to
treat slice-like properties specially in the presence of writeback, with labels
``enter_inout:`` and ``exit_inout:``. Copy-on-write containers can then use
these labels to maintain the inout reference count of their backing stores and
optimize in-place mutation of ``@inout`` slices.

Other Considered Solutions
--------------------------

Move optimization seemed like a potential solution when we first considered
this problem--given that it is already unspecified to reference a property
while an active ``@inout`` reference can modify it, it seems natural to move
ownership of the value to the ``@inout`` when entering writeback and move it
back to the original value when exiting writeback. We do not think it is viable
for the following reasons:

- In general, relying on optimizations to provide performance semantics is
  brittle.
- Move optimization would not be memory safe if either the original value or
  ``@inout`` slice were modified to give up ownership of the original backing
  store.  Although observing a value while it has inout aliases is unspecified,
  it should remain memory-safe to do so. This should remain memory safe, albeit
  unspecified::

    var arr = [1,2,3]
    func mutate(x: @inout Int[]) -> Int[] {
      x = [3..4]
      return arr[0..2]
    }
    mutate(&arr[0..2])

  Inout slices thus require strong ownership of the backing store independent
  of the original object, which must also keep strong ownership of the backing
  store.
- Move optimization requires unique referencing and would fail when there are
  multiple concurrent, non-overlapping ``@inout`` slices. ``swap(&x.a, &x.b)``
  is well-defined if ``x.a`` and ``x.b`` do not access overlapping state, and
  so should ``swap(&x[0..50], &x[50..100])``.  More generally, we would like to
  use inout slicing to implement divide-and- conquer parallel algorithms, as
  in::

    async { mutate(&arr[0..50]) }
    async { mutate(&arr[50..100]) }

enter_inout and exit_inout
==========================

We propose making an optional pair of labels to computed property definitions,
``enter_inout`` and ``exit_inout``. When a property has these labels defined
and is referenced in a context that requires writeback, the ``enter_inout``
method is applied immediately after ``get``, and the ``exit_inout`` method is
applied **to the value at the time of get** immediately before ``set``.
This operation::

  mutate(&arr[a..b])

thus behaves as if by the following sequence of calls when
``enter_inout`` and ``exit_inout`` are present for the property::

  var slice = arr.subscript(a..b).get()
  var arr_orig = arr
  arr_orig.subscript(a..b).enter_inout()
  mutate(&slice)
  arr_orig.subscript(a..b).exit_inout()
  arr.subscript(a..b).set(slice)

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
    /// for the originating non-@inout value.
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
