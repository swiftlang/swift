==============================================
 Strings, Mutability, and In-Place Operations
==============================================

:Author: Dave Abrahams
:Author: Joe Groff

**Abstract**: The design of Strings has revealed some misconceptions
  we held in the past, and leads us to a general design for handling
  in-place operations analgous to ``+=``.  This paper discusses the
  thinking behind the current design and proposes a language extension
  for in-place operation support.

String Mutation
===============

Should Swift ``String``\ s be immutable? Even if the backing store is
immutable, the values themselves could still be reassigned and
swapped.  Therefore, there's really no choice: ``String``\ s **are
mutable**.

We can also ask if it makes sense to *limit* mutations to those that
can be expressed as wholesale assignments, but that question turns out
to be meaningless, because *any* mutation of a ``String`` can be
expressed in terms of a wholesale assignment.  Even if we tried to
impose an “assignment-only” limitation, I'd still be free to write::

  extension String {
    func inplace_upper() {
        this = this.upper()
    }
  }

The ``inplace_upper`` implementation above is semantically
indistinguishable from one that's written in terms of by-part
mutations.  We never pass out *logical* references to the underlying
string buffer—even though the buffer may be shared by many strings,
each ``String`` instance presents an logically-independent value.

In-Place Mutations
==================

Once we allow assignment and concatenation via ``a.s1 + b.s2``\ —which
creates a new ``String``\ —it clearly makes sense to also allow ``a.s1
+= b.s2``\ —which modifies a ``String`` in place.  However, there are
many operations for which “create a new string” and “modify in place”
variants both make sense, but don't have distinct, concise, accepted
spellings.  For example, does ``s.upper()`` modify ``s`` in-place, or
does it create a new string value that can only be used to overwrite
``s`` via ``s = s.upper()``?

.. Note:: We could also present both interfaces, using a canonical
          naming relationship for creating and mutating variants
          like the one we have for the (inplace) operators.  We'll
          explore that approach—which has the obvious downside of
          complicating the API—after working through this one.

Creating or Mutating?
=====================

From a usability point-of-view, this question answers itself fairly
easily.  With a creating ``upper()``, we get::

  var y = x.upper()           // y is an upcased copy of x

  x = x.upper()               // upcase x "in-place"

  var z = f().upper().split() // compose operations

With a mutating ``upper()``, we get::

  var y = x.copy()   // y is going to be an upcased copy of x...
  y.upper()          // ...eventually

  x.upper()          // upcase x in place

  var z = f()      // operations don't compose
  z.upper()
  z.split()

The creating interfaces are a clear usability win.  The minor
inconvenience of assigning ``x.upper()`` into ``x`` is more than
outweighed by the disadvantages of the mutating interface:

1. Verbosity

2. The need to introduce a named temporary

3. Spurious mutations of ``y`` and ``tmp``, which are conceptually
   costly.  If we eventually get immutability in the type system,
   we still won't be able to label ``y`` immutable

One could attempt to address the first two issues by making mutating
operations chainable, but we believe that only replaces one set of
problems with new ones.  The third issue, we believe, is an inevitable
symptom of using a mutating operation.

The Argument for Mutating Operations
====================================

Although, if we had to choose, we would choose creating operations,
there *are* good arguments for their mutating variants.  For example,
if you want to do an in-place modification on something that's verbose
to access, ::

   some.thing().that_is.verbose().to_access.inplace_upper()

is a lot cleaner than either of these approaches::

   some.thing().that_is.verbose().to_access
   = some.thing().that_is.verbose().to_access.upper()

   var tmp = some.thing().that_is.verbose()
   tmp.to_access = tmp.to_access.upper()

Furthermore, ``x = x.upper()`` causes an allocation/deallocation pair
and data copying that can be avoided with a mutating interface
and are are unlikely to be optimized away by even a clever compiler.

.. Admonition:: It's not just about ``String``\ s

   We stipulate that it's possible in the compiler to implement
   special-case optimizations for ``String``, but all of these
   arguments apply to other types as well.  We recommend getting the
   general feature we're proposing into the core language and leaving
   these optimizations to the library wherever possible.

Copy On Write
=============

Once we agree that mutating operations are viable, we can also agree
that copy-on-write is a viable optimization for mutating operations in
those cases where the string's buffer is uniquely referenced::

  struct String {
    ...

    func inplace_upper() {
      this.unique()                  // copy buffer iff refcount > 1
      for i in 0..buffer.length {
        buffer[i].inplace_upper()    // naïve ASCII-only implementation
      }
    }
    ...

  }

Ponies for Everyone!
====================

When considering ways to present both mutating and creating
interfaces, we considered several possibilities.  The leading
candidates fell into two basic schemes: either use methods for one
semantics and “free functions” for the other, or simply choose two
different names.

Using “Method-ness” to Distinguish Semantics
--------------------------------------------

There are two choices.

1. “Methods Mutate”::

     var y = upper(x)     // creating
     x.upper()            // mutating

   This approach fits with the OOP-ish expectation that methods have
   special privileges to mutate an instance.  However, it sacrifices
   the ability to chain create methods, an important syntactic
   advantage.  Instead we must use nested calls::

    var z = split( trim( upper(x) ) ) // composition

2. “Methods Create”::

     var y = x.upper()                // creating
     upper(&x)                        // mutating
     var z = x.upper().trim().split() // composition

   Here, composition is nicer: it reads left-to-right and without
   conceptual nesting.  That said, the prevalent mental association of
   methods with access control may make it harder for our audience to
   swallow, and it has the disadvantage that when you type “up” in an
   IDE, code completion will have to show you all the functions whose
   names begin with “up,” rather than just those that apply to
   ``String``.

Tying Semantics to a Naming Convention
--------------------------------------

The precedent for this approach has already been set by the binary
operators.  The only question is, what should the convention be?  The
two categories here are:

1. Mutating operations get the short name::

     var y = x.uppered()                      // creating
     x.upper()                                // mutating
     var z = x.uppered().trimmed().splitted() // composed
     
2. Creating operations get the short name::

     var y = x.upper()                // creating
     x.inplace_upper()                // mutating
     var z = x.upper().trim().split() // composed

Because the creating interface is the right choice `in so many
cases`__ and because it will appear repeatedly in a single statement
compositions, we favor design #2.

__ creating-or-mutating-_

Optimization and Convenience
============================

A discussion of two language features:

1. Generating ``x.upper()`` from
   ``x.inplace_upper()`` and vice-versa (as with ``+`` and ``+=``) 

2. Optimizing ``x = x.upper()`` into ``x.inplace_upper()``

TODO: write me
