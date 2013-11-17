==================
 Operator Methods
==================

.. Note:: This is **not** proposed as a 1.0 feature.

The Problem
===========

One of the main reasons we decided to go with something like the
``Indexable`` protocol instead of a D-like “everything is a sequence”
model is that we thought ``Indexable`` would be more familiar: we
could let people write algorithms the way they do with arrays:

.. parsed-literal::

  extension Indexable {
    func find<Seq: Indexable where Seq.Element: Equatable>(
      s: Seq, findme: Seq.Element) -> Seq.Index
    {
      for var i = s.start; i != s.end; **++i** {
        if **s[i]** == findme {
          return i
        }
      }
      return s.end
    }
  }
   
The problem is that in order to move some indices, like those for
``String`` (which stores variable-length grapheme clusters) and
``Dictionary`` (which uses open addressing and therefore contains
“holes”), we need to examine the container's data.  That's unfriendly
to expressions like ``++i``, which have no access to the container.

We could store a reference to the container's data in the index, but
given our copy-on-write model, when mutating algorithms are working
with indices, there is never a unique reference to the container's
data, which forces the data to be copied with each mutation.

The Solution
============

Therefore, we're giving the container responsibility for moving
indices:

.. parsed-literal::

  func find<Seq: Indexable where Seq.Element: Equatable>(
    s: Seq, findme: Seq.Element) -> Seq.Index
  {
    for var i = s.start; i != s.end; i = **s.succ(i)** {
      if s[i] == findme {
        return i
      }
    }
    return s.end
  }

Unfortunately the algorithm is now starting to look unfamiliar.  It
gets a bit better if we can make ``find`` a method on the
``Indexable`` protocol, because the involvement of the container
becomes implicit:

.. parsed-literal::

  extension Indexable where Self.Element: Equatable {
    func find(findme: Seq.Element) -> Seq.Index
    {
      for var i = **start**; i != **end**; **i = succ(i)** {
        if **self[i]** == findme {
          return i
        }
      }
      return s.end
    }
  }

If we could only declare ``++`` to be a *method* of ``Indexable``, we
could use the implicit ``self`` feature of methods *and* recover a
completely natural syntax:
  
.. parsed-literal::

  protocol Indexable {
    typealias Index
    typealias Element
    **@method, @assignment, @prefix func ++(_: @inout Index)**
    func subscript(_:Index) -> Element
  }

  extension Indexable where Self.Element: Equatable {
    func find(findme: Seq.Element) -> Seq.Index
    {
      for var i = start; i != end; **++i** {
        if **self[i]** == findme {
          return i
        }
      }
      return s.end
    }
  }

