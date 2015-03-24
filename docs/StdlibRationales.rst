:orphan:

.. @raise litre.TestsAreMissing

=================================================
Rationales for the Swift standard library designs
=================================================

This document collects rationales for the Swift standard library.  It is not
meant to document all possible designs that we considered, but might describe
some of those, when important to explain the design that was chosen.

Current designs
===============

Some ``NSString`` APIs are mirrored on ``String``
-------------------------------------------------

There was not enough time in Swift 1.0 to design a rich ``String`` API, so we
reimplemented most of ``NSString`` APIs on ``String`` for parity.  This brought
the exact ``NSString`` semantics of those APIs, for example, treatment of
Unicode or behavior in edge cases (for example, empty strings), which we might
want to reconsider.

Radars: rdar://problem/19705854

``size_t`` is unsigned, but it is imported as ``Int``
-----------------------------------------------------

Converging APIs to use ``Int`` as the default integer type allows users to
write fewer explicit type conversions.

Importing ``size_t`` as a signed ``Int`` type would not be a problem for 64-bit
platforms.  The only concern is about 32-bit platforms, and only about
operating on array-like data structures that span more than half of the address
space.  Even today, in 2015, there are enough 32-bit platforms that are still
interesting, and x32 ABIs for 64-bit CPUs are also important.  We agree that
32-bit platforms are important, but the usecase for an unsigned ``size_t`` on
32-bit platforms is pretty marginal, and for code that nevertheless needs to do
that there is always the option of doing a bitcast to ``UInt`` or using C.

Type Conversions
----------------

The canonical way to convert from an instance `x` of type ``T`` to
type ``U`` is ``U(x)``, a precedent set by ``Int(value: UInt32)``.
Conversions that can fail should use failable initializers,
e.g. ``Int(text: String)``, yielding a ``Int?``. When other forms provide
added convenience, they may be provided as well. For example::

  String.Index(s.utf16.startIndex.successor(), within: s) // canonical
  s.utf16.startIndex.successor().samePositionIn(s)        // alternate

Converting initializers generally take one parameter. A converting
initializer's first parameter should not have an argument label unless
it indicates a lossy, non-typesafe, or non-standard conversion method,
e.g. ``Int(bitPattern: someUInt)``.  When a converting initializer
requires a parameter for context, it should not come first, and
generally *should* use a keyword.  For example, ``String(33, radix:
2)``.

:Rationale: First, type conversions are typical trouble spots, and we
   like the idea that people are explicit about the types to which
   they're converting.  Secondly, avoiding method or property syntax
   provides a distinct context for code completion.  Rather than
   appearing in completions offered after ``.``, for example, the
   available conversions could show up whenever the user hit the “tab”
   key after an expression.

Possible future directions
==========================

This section describes some of the possible future designs that we have
discussed.  Some might get dismissed, others might become full proposals and
get implemented.

Mixed-type fixed-point arithmetic
---------------------------------

Radars: rdar://problem/18812545 rdar://problem/18812365

Standard library only defines arithmetic operators for LHS and RHS that have
matching types.  It might be useful to allow users to mix types.

There are multiple approaches:

* AIR model,

* overloads in the standard library for operations that are always safe and
  can't trap (e.g., comparisons),

* overloads in the standard library for all operations.

TODO: describe advantages

The arguments towards not doing any of these, at least in the short term:

* demand might be lower than we think: seems like users have converged towards
  using ``Int`` as the default integer type.

* mitigation: import good C APIs that use appropriate typedefs for
  unsigned integers (``size_t`` for example) as ``Int``.


Swift: Power operator
---------------------

Radars: rdar://problem/17283778

It would be very useful to have a power operator in Swift.  We want to make
code look as close as possible to the domain notation, the two-dimensional
formula in this case.  In the two-dimensional representation exponentiation is
represented by a change in formatting.  With ``pow()``, once you see the comma,
you have to scan to the left and count parentheses to even understand that
there is a ``pow()`` there.

The biggest concern is that adding an operator has a high barrier.
Nevertheless, we agree ``**`` is the right way to spell it, if we were to have
it.  Also there was some agreement that if we did not put this operator in the
core library (so that you won't get it by default), it would become much more
compelling.

We will revisit the discussion when we have submodules for the standard
library, in one form or the other.

