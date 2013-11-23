Hacking on Swift
================

.. contents::

Abstract
--------

The point of this document is to collect folk wisdom about Swift
development.  It's folk wisdom if:

- You really need to understand it if you're hacking on a particular
  subsystem.

- It doesn't really fit in any of the specification documents:

  - the Swift language reference

  - the SIL language reference

  - the ABI specification

- It feels too broad for just a comment in the code somewhere.

By implication, anything that *does* fit into one of those places
should just go there with, at most, a cross-reference here.

This document is broken down into different sections for different
subsystems.

The AST
-------

Types
~~~~~

The Type hierarchy serves a number of masters:

- the Swift system of *formal types*,

- the SIL system of *lowered types*, and

- intermediate scratch types required by Sema's constraint systems.

A *formal type* is the type of something in Swift, like an expression
or declaration.

Type Variables
``````````````

A *type variable* stands for an unknown type.  There are several
different kinds of type variables:

- ``TypeVariableType`` is used for the temporary type variables
  introduced as part of generating and solving constraints in the type
  checker.  These variables should all be resolved by the solver
  eventually in a well-formed program, leaving all AST nodes totally
  free of TypeVariableTypes after type-checking.

- ``ArchetypeType`` is used within the implementation of generic
  declarations to represent the rigid type parameters of context and
  their associated types.  For example::

    func swap<T>(a : @inout T, b : @inout T) {
       var tmp = a
       b = a
       a = tmp
    }

  Within this context, ``T`` is an archetype; the ``VarDecl``\ s for
  ``a``, ``b``, and ``c`` (and expressions referring to them) will
  have that type.  Similarly, in the SIL body of this function, the
  instructions will use an archetype type.

  For now, archetypes are also used in generic signatures as seen
  externally: that is, ``swap->getType()`` will yield a
  ``PolymorphicFunctionType`` whose input type is a tuple of l-value
  types of archetypes.  But this is being actively changed, and these
  archetypes will eventually become ``GenericTypeParamType``\ s and
  ``AssociatedTypeType``\ s.

- ``GenericTypeParamType`` and ``AssociatedTypeType`` are used within
  the signature of a generic declaration to represent its rigid type
  parameters and their associated types.

Substituted Types
`````````````````

A type can be *substituted* or *unsubstituted*.  A substituted type is
expressed using only types meaningful in the current context.  An
unsubstituted type may be expressed using types meaningful only in
a different context.

An expression or local declaration will always have a substituted type.

A declaration from a different context will have an unsubstituted
type, although this might be trivially identifical to a
correctly-substituted type.

For example::

  struct Dictionary<T, U> { ... }

  func lookupIterative<X>(value: X, map: Dictionary<X, X>) {
    while var newValue = map[value] {
      value = newValue
    }
    return value
  }

``Dictionary``\'s subscript getter will have formal unsubstituted type
``<T,U> (@inout Dictionary<T,U>) -> (T) -> U?``.  The substituted use
of it has type ``(@inout Dictionary<X,X>) -> (X) -> X?``.  The
index expression has type ``X``, and the unsubstituted parameter type
correspond to that is ``T``.

Declarations
~~~~~~~~~~~~

A declaration's *interface type* is its type "as seen from outside".

A declaration's *r-value type* is the formal type of an r-value
reference to it.  This is just the type it was declared with.

A variable's *storage type* is the formal type of its storage.  It
differs from the r-value type only for declarations with ownership:

- A ``@weak`` variable of type ``T?`` has storage type
  ``WeakStorageType(T)``, i.e. ``@sil_weak T``.

- An ``@unowned`` variable of type ``T`` has storage type
  ``UnownedStorageType<T>``, i.e. ``@sil_unowned T``.

``VD->getType()`` yields an unsubstituted storage type.

Parsing
-------

Semantic Analysis
-----------------

Serialization
-------------

SIL
---

Type Lowering
~~~~~~~~~~~~~

An *uncurried type* is a formal function type which has had two or
more of its formal input clauses combined into a single tuple input
clause.  The exact transformation is ``A -> B -> C -> D`` to ``(C,
B, A) -> D``.  Note that doing this in parts is not equivalent: ``(C,
(B, A)) -> D`` is a different type.

A *bridged type* has had native Swift representations of types turned
into foreign equivalents.  For example, ``String`` might turn into
``NSString``.  Type bridging only affects function types with a
foreign abstract CC.

An uncurried, bridged function type is essentially one step away from
being a lowered SIL function type.

However, the lowered type of a declaration is not necessarily the same
as the lowering of its uncurried, bridged function type.  Type
lowerings always use the standard conventions for their abstract CCs,
but specific functions may use non-standard conventions.

SIL type lowering does the following manipulations:

- tuples are element-wise lowered and then reconstructed

- function parameter types are deeply exploded ("de-tupled") and element-wise lowered

- function result types are lowered (and turned into a parameter if address-only)

Or at least, that's how it effectively plays out when there isn't an
abstraction pattern.  What actually happens is that we simultaneously
walk into the abstraction pattern, decide how *that* would be
represented, and then expand stuff according to it.  When the
abstraction pattern is totally opaque, we just throw up our hands and
do the worst thing we can possibly imagine.

Abstraction Difference
~~~~~~~~~~~~~~~~~~~~~~

The basic principle for the soundness of abstraction difference in SIL
is that, as long as type substitution is done respecting abstraction
difference, it should be okay to work with a single value at any level
of substitution.

This principle means that, any time you're working with a value whose
type in the local context has been derived from substituting the type
of a generic entity (e.g. a generic function or a member variable of a
generic type), you need to derive its lowered type in one of two ways:

- Lower the substituted formal type using the original formal type as
  an abstraction pattern.  The substituted type needs to actually be a
  substitution of the original type, differing only in things like
  top-level polymorphism.  This is generally the easiest thing, and
  you'll want these types anyway if you need to do conversions.

- Use SIL type substitution (not Swift type substitution!) on the
  lowered original formal type.

SIL Generation
--------------

``emitOrigToSubstValue`` transforms a value that's abstracted according to
the original abstraction conventions into a value that's abstracted
according to the substituted abstraction conventions.  That is, it
turns ``@callee_owned (@out (), @in ()) -> ()`` into
``@callee_owned () -> ()``.

``emitSubstToOrigValue`` is just the reverse of that.

Both require you to give the original and subst formal types,
uncurried where applicable.  It's theoretically possible to do
re-abstraction based on lowered types, but what I've found is that, if
you try, you will pretty quick get stuck dealing with endless problems
involving empty tuple types.  Having the formal types around makes
this brain-dead.

When you're making a re-abstraction thunk, you need to be able to
reverse a transformation; for example, if you orig-to-subst ``(T ->
U)`` into ``((Int -> Float) -> Float)``, the ensuing thunk actually
does a subst-to-orig on its ``Int -> Float`` parameter to turn it into
a ``@callee_owned (@out Float, @in Int) -> ()``, then does an
orig-to-subst on the result to turn the indirect return into a direct
one.

IR Generation
-------------

*Explosion levels* are used to make IR-generation more explicit about
when it can get away with passing values around directly that might be
resilient in a different resilience domain.  It's probably not the
right abstraction for this, though.

An *explosion* is a linear collection of LLVM IR scalar values.  Swift
has a number of types (primitive and otherwise) that comprise multiple
IR values; ``Explosion`` makes it much easier to pass them around
without artificially turning them into first-class aggregates.

An explosion is first filled, then drained.  Extra elements cannot be
added after the first element is claimed.  As a sanity check,
explosions must be drained to completion.

Any given explosion may contain several concatenated values at once,
so a type-specific operation that consumes its inputs out of an
explosion should take care to only consume the values that belong to
that type.  For example, an operation on a type whose explosion schema
is three pointer values should always claim exactly three values.

``claimAll()`` and ``reset()`` should be used only very carefully.
