=================================
 An Mutation Model for Swift 1.0
=================================

:TL;DR: By declaring properties and ``subscript`` methods with no
        setter, we can create values that can't be directly mutated,
        and by using ``@inout``, we can express whether a function
        mutates a value.  This partial mutation model already exists
        in the language, but is incomplete.  This proposal completes
        the model and suggests several possible directions in which
        it could be extended.

:Acknowledgement: This proposal is a refinement and partitioning of an
                  idea that's long been advocated by John McCall.
           
The Problem
===========

Consider::

 class Window {

   var title: String {
   get:                          // No setter!
     return somethingComputed()
   }
 }

  var w = Window()
  w.title += " (parenthesized remark)”

What do we do with this?  Since ``+=`` has an ``@inout`` first
argument, we detect this situation statically (hopefully one day we’ll
have a better error message): [#append]_

::
   
 <REPL Input>:1:9: error: expression does not type-check
 w.title += " (parenthesized remark)"
 ~~~~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~~~~

Great.  Now what about this? ::

  w.title.append(" (fool the compiler)")

Today, we allow it, but since there’s no way to implement the
write-back onto ``w.title``, the changes are silently dropped.

Not-Recommended Approaches
==========================

.. admonition:: Spoiler alert

                Nothing in this section works out very well

Treat All Method Calls as Mutations
-----------------------------------

Since the self parameter of a method on a value type is implicitly
passed as ``@inout``, we could choose to treat this case consistently
and diagnose it as an error.  But then, we have no way to allow this
*non-mutating* method call:

.. parsed-literal::

   var words = w.title.\ **split()**

or, pretty much any other non-mutating access to ``w.title``, without
forcing the user to create a temporary

.. parsed-literal::

   **var title** = w.title
   var words = title.split()

As a result, it becomes **impossible to “method-chain”** innocuous
non-mutating operations, which I think is probably unacceptable.

Ban ``get:``\ -Only Properties of Value Type
--------------------------------------------

The upshot of this decision would be that these get-only properties ::

 var x = w.children\ **.length**
 var i = someString\ **.startIndex**

would have to be written as methods:

.. parsed-literal::

   var x = w.children\ **.length()**
   var i = someString\ **.startIndex()**

As first, this doesn't seem too onerous, but the ripple effects
continue.  For example, any get-only properties of value type exposed
by Objective C would have to be presented as methods in Swift, and
we’d also have to ban get-only properties of generic type unless the
types are constrained to be classes.

Status Quo
----------

This approach has the great advantage of requiring no new compiler
work.  It has the obvious problem that perfectly sensible-looking code
fails to fulfill the user's intention even though the static type
system has all the information it needs to diagnose the issue.

Recommended Approach
====================

The Basics
----------

The rules described in this section are enough to solve the stated
problem.  Following sections cover our additional options.

1. Allow users to declare a method as ``@inout``, meaning that it
   mutates the target.

   .. parsed-literal::

      struct String {
        **@inout** def append(s: String) {
          self += s
        }
      }

  The ``self`` parameter to a method not so labelled would no longer
  considered implicitly ``@inout``.
      
  [This attribute is meaningless on methods of types known to be
  classes; how and whether to diagnose that use is TBD].

2. Make it an error to use a mutating method on a value-typed
   ``get:``\ -only property or on the value-typed result of using a
   ``get:``\ -only ``subscript`` method.

3. Make it an error for a non-\ ``@inout`` method to call an
   ``@inout`` method on the target object.

Optimizing Based On Non-Mutation
--------------------------------

.. Note:: This is an optional part of the proposal

Imposing a rule for the design of sound value types would allow us to
make optimizations based on static knowledge of mutation.  In
particular, access to the properties and subscripts of value types
would not be allowed to act as references onto shared mutable state.

Without this guarantee, we can’t avoid repeatedly calling a property’s
getter even when its target object is known not to be
modified. [#slice]_

There are probably further optimizations that could be extracted by
imposing other semantic rules, but discovering these should probably
be driven by what looks like an opportunity to the core compiler team.
Unfortunately, all such decisions **should probably be made early**
because it is impossible to automate the enforcement of such rules
with tools, and it's much easier to lift restrictions on users than
add them.

Declaring Immutable Data
------------------------

.. Note:: This is an optional part of the proposal

As Joe Groff has clarified for me, the rest of this proposal does not
deal with “immutability” in the strongest sense, because a
``get:``\ -only property such as an ``Array``\ 's ``.count`` can be
changed by other (mutating) methods of the target object.  This
section deals with true immutability.

John McCall has proposed that in addition to always-mutable ``var``\
s, we allow the declaration of ``val``\ s, which are immutable after
construction.  His suggestion integrates in obvious ways with the rest
of this proposal: calling an ``@inout`` method on a ``val`` is simply
prohibited.  Taking the optimization opportunity of the previous
suggestion also requires prohibiting assignment to a property or to a
subscript of a ``val``.

John has thought through ``val`` much more than I have, so I defer
further elaboration to him.

Summary
=======

The fundamental problem here is that we’ve given people ways express
mutation and to prevent assignment but have not extended the
expression of mutation to target objects, leaving a hole.  This
proposal closes the hole and suggests two additional improvements.
Thanks for reading!

--------

.. [#append] String will acquire an ``append(other: String)`` method as part of the
             formatting plan, but this scenario applies equally to any
             method of a value type

.. [#slice] The current ``Slice<T>`` is banned by this rule, but
            that's okay because we're replacing it with ``Array<T>``
