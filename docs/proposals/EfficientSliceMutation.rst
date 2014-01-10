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

We propose to create a special ``@inout`` subscript operations that
can be used to treat slices specially in the presence of writeback,
with labels ``inout_set:`` and ``inout_get:``.

We propose that for arrays created natively in Swift, the underlying
buffer's "weak" reference should be repurposed to count ``@inout``
references.  When all strong references but one are accounted for by
``@inout`` references, there is no need to copy the underlying buffer
prior to modification.

