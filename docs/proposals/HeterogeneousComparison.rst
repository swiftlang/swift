:orphan:

In the standard library, we currently use the ``Equatable`` and
``Comparable`` protocols to model value identity and total ordering for data
structures.  This is nice and simple, but has some drawbacks in Swift's model
that we should address before stabilizing the standard library API.

Heterogeneous Collections
=========================

A key weakness is support for *heterogeneous collections*, particularly
when using collections of protocol type. ``Equatable`` and ``Comparable``
both enforce strong typing by imposing ``Self`` type requirements on the ``==``
and ``<`` operators. Currently this prevents the use of these protocols as
existential types, and more insidiously, prevents any protocols from refining
``Equatable`` or ``Comparable`` without having the same constraint, making it
impossible to directly use protocol types in ``Dictionary``, ``Set``, or with
equality- and ordering-related generic operations such as ``indexOf`` and
``sort``. A number of people have run into this problems, including DaveA
in his WWDC15 talk, and Brent Simmons in his "Swift Diary" series:
http://inessential.com/swiftdiary

In theory, the ``Self`` requirement in ``Equatable`` and ``Comparable`` isn't
strictly necessary; both equality and ordering can be generalized from
operations within individual ``Equatable`` or ``Comparable`` types to operations
between values of different ``Equatable`` or ``Comparable`` types::

  // True if `x` and `y` are the same type and `x == y`, false otherwise.
  func heterogeneouslyEqual<T: Equatable, U: Equatable>(x: T, _ y: U) -> Bool {
    if let yt = y as? T {
      return x == yt
    }
    return false
  }

  // Order values of different types by assigning an arbitrary per-process
  // total ordering of types, and producing the lexicographical comparison
  // (T, x) < (U, y).
  func heterogeneouslyLT<T: Comparable, U: Comparable>(x: T, _ y: U) -> Bool {
    if let yt = y as? T {
      return x < yt
    }
    return ObjectIdentifier(T.self).uintValue
         < ObjectIdentifier(U.self).uintValue
  }

However, it would not be ideal for this to be the standard behavior for ``==``
and ``<``, since it would mask type errors::

  0 == 0.0 // quietly returns false
  1 < "2" // quietly returns however Int and String are ordered

Domain-specific Comparison Behavior
===================================

Beyond heterogeneous comparison, there are other situations where
conventional or domain-specific meanings of ``<`` and ``==`` are at odds with
the requirements of collections. Some other potential examples include:

- The most infamous of these is IEEE floating-point, which defines ``NaN`` to
  be unordered, and considers ``+0.0`` and ``-0.0`` to be equal despite having
  different semantics. One could assign a total ordering to floats that
  includes ``NaN`` and distinguishes positive and negative zero, allowing them
  to be used reliably in collections, but that behavior would run contrary to
  the conventional meaning of ``<`` and ``==`` applied to floats. 

- Types and other runtime-uniqued data structures can be given a cheap
  per-process total ordering based on pointer equality, which would be
  sufficient for the needs of an ordered data structure, much like a per-
  process hash function can be. However, nondeterministic behavior across runs
  is a weaker guarantee than most types provide for ``Comparable``.

- ``Optional`` assigns ``nil`` a total ordering relative to ``Some`` value of
  the wrapped type. This is nice to allow ordered collections of ``Optional``,
  but ``.Some(1) < nil`` having a meaning has been surprising to users.

- Similarly, vectors and complex numbers don't have a unique total ordering as
  mathematical entities, and it would be undesirable for ``<`` to work on
  them. As programming constructs, though, they can use a lexicographical
  ordering of their components as a total ordering sufficient for use in
  ordered data structures.

- Strings have several possible orderings at different levels of abstraction.
  Unicode collation order might make sense for ``<`` and ``==``, but is
  relatively expensive. A total ordering sufficient for ordered data structures
  could potentially be provided more cheaply by normalized unicode-scalar
  ordering.

Proposal: Separate "Value-Oriented" and "Container-Oriented" Comparison
=======================================================================

I think that the above considerations indicate a tension between the
expectations of how operators like ``==`` and ``<`` behave on individual values
and the requirements of collections. For the former operations, which I'll
refer to as "value-oriented", the following characteristics are desirable:

- Type safety. It should be a compile-time error to apply ``==`` or ``<`` to
  different-typed operands. More general heterogeneous operations should be
  opted in to.
- Stable behavior across runs. Collections could take advantage of per-process
  characteristics like pointer identity for ordering, but it would be
  extremely surprising for ``<`` to behave nondeterministically in general.
- Adherence to domain-specific behavior for the type, such as Float's
  ``+0.0 == -0.0`` and ``NaN != NaN``.

On the other hand, "container-oriented" ordering has different priorities:

- Heterogeneous containers should be easy to use.
- Stability across runs is less important; users are familiar with
  nondeterministic data structures such as hash tables.
- Collections benefit from strict adherence to the mathematical properties
  of total ordering and equivalence, independent of any conventions specific
  to a particular type.

I propose that we offer different protocols for value- and container-oriented
comparison, related via protocol extensions instead of by protocol refinement.
The value-oriented protocols, ``Equatable`` and ``Comparable``, can stay
pretty much as is, requiring strict ``Self`` type matching of operands::

  protocol Equatable {
    func ==(_: Self, _: Self) -> Bool
    func !=(_: Self, _: Self) -> Bool
  }
  protocol Comparable: Equatable {
    func <(_: Self, _: Self) -> Bool
    func >(_: Self, _: Self) -> Bool
    func <=(_: Self, _: Self) -> Bool
    func >=(_: Self, _: Self) -> Bool
  }

``Hashable``, however, is a container-oriented operation. Instead of
refining ``Equatable``, it should provide its own heterogeneous predicate
method::

  protocol Hashable {
    var hash: Int { get }

    /// True if `x` is equivalent to `self` as a member of a hashed collection.
    ///
    /// If the `Self` type also conforms to `Equatable`, a default
    /// implementation of `matches` is provided.
    ///
    /// Axiom: x.matches(y) implies x.hash == y.hash
    /// (`matches` name open to bikeshedding)
    func matches<T: Hashable>(x: T) -> Bool
  }

which can receive a default implementation for types that are also
``Equatable``::

  extension Hashable where Self: Equatable {
    /// Return true if `x` is the same type as `self`, and `x == self`.
    func matches<T: Hashable>(x: T) -> Bool {
      if let xt = x as? T {
        return self == xt
      }
      return false
    }
  }

Instead of using ``Comparable`` to represent a total ordering for containers,
we can provide a container-oriented ``Orderable`` protocol (name open to
bikeshedding), with a heterogeneous order comparison operation::

  enum Ordering { case Descending, Same, Ascending }

  protocol Orderable {
    /// Return the relative order of `self` and `x`.
    ///
    /// If there is no specialized ordering between values of type `Self` and
    /// `T`, the implementation should delegate to:
    ///
    ///   self.dynamicType.compare(x.dynamicType)
    ///
    /// to order the values by type.
    ///
    /// If the `Self` type also conforms to `Comparable`, then a default
    /// implementation is provided.
    ///
    /// This ordering is not guaranteed to be stable across process invocations,
    /// so should not be relied on for serialization or data transmission
    /// purposes, but is guaranteed to be stable within the current process.
    func compare<T: Orderable>(x: T) -> Ordering
  }
  extension Orderable {
    /// Return the relative order of `Self` and `T`.
    ///
    /// This ordering is not guaranteed to be stable across process invocations,
    /// so should not be relied on for serialization or data transmission
    /// purposes, but is guaranteed to be stable within the current process.
    static func compare<T: Orderable>(x: T.Type) -> Ordering {
      return ObjectIdentifier(self).uintValue
        .compare(ObjectIdentifier(x).uintValue)
    }
  }

and a default implementation for ``Comparable`` types::

  extension Orderable where Self: Comparable {
    func compare<T: Orderable>(x: T) -> Ordering {
      if let xt = x as? T {
        return self <  xt ? .Ascending
             : self == xt ? .Same
             :              .Descending
      }
      return self.dynamicType.compare(x.dynamicType)
    }
  }

Since neither ``Hashable`` nor ``Orderable`` introduce ``Self`` type constraints
in this model, other protocols can refine them and still be usable as
existential types, and it should be possible to use protocol types that inherit
``Hashable`` and ``Orderable`` as elements of heterogeneous ``Set`` or
``Dictionary`` containers (once we implement protocol type self-conformance).

There are some obvious drawbacks to this design (and likely some nonobvious
ones). It adds new APIs with very similar meanings to existing ones, which
could be confusing. Types also have to declare separate conformance to both
``Equatable`` and ``Hashable``, and ``Comparable`` and ``Orderable``, if they
want both behaviors. However, most users aren't writing their own data
structures, and the default implementations of ``Hashable.matches`` and
``Orderable.compare`` are hopefully sufficient for most types, so typical
users only have to be aware of the protocols and not their container-oriented
methods.

Alternative Solution: Improved Protocol Existentials
====================================================

The fact that ``Self`` and associated type constraints prevent the use of
protocols as existential types is an unfortunate language limitation that
we want to eventually lift. As noted above, the behavior of ``Equatable`` and
``Comparable`` can be generalized from same-type operations to heterogeneous
operations. It would be theoretically possible to allow extensions to
protocol existential types to specify how they can conform to their own
protocol when contravariant constraints make it nonobvious how to do so.
As strawman syntax, this could be written as an extension on ``protocol<P>``::

  /// Extend the Equatable existential type to conform to the Equatable
  /// protocol.
  extension protocol<Equatable>: Equatable {
    func ==(x: Equatable, y: Equatable) -> Bool {
      return x._equals(y)
    }
  }

  extension Equatable {
    func _equals(y: Equatable) -> Bool {
      if let ySelf = y as? Self {
        return self == ySelf
      }
    }
  }

However, even if this were possible, I'm not sure it's the right solution to
the heterogeneous collection problem. It addresses the language limitation
that prevents ``Equatable``-derived protocols from being usable as dynamic
types, but introduces other problems:

- As noted, it is desirable in most cases for ``==`` and ``<`` to be strongly
  typed. Since protocol existentials are supertypes of conforming types,
  making the ``Equatable`` and ``Comparable`` existentials themselves
  ``Equatable`` and ``Comparable`` would defeat the type safety of their
  operators, and allow expressions like ``0 == 0.0`` and ``1 < "2"`` to
  quietly compile with unexpected behavior. Users have already run in to
  trouble due to ``Optional`` 's ``<`` overload quietly accepting comparisons
  between unchecked optionals due to ``Optional`` subtyping.
- This design doesn't allow for customization of heterogeneous comparison.
  If you're comparing two values of the same concrete type, you'll get
  that type's operators, but if you compare different types, only the
  ``Equatable`` existential's extension operators apply. There's no way
  for a related family of types to provide custom ordering behavior among the
  related types, which could be done with the separated ``Hashable`` and
  ``Orderable`` protocols. Furthermore, it prevents any protocol that
  refines ``Equatable`` from extending its own existential to conform to
  ``Equatable`` in a different way, since as a subtype of ``Equatable``, it
  would inherit the ``Equatable`` existential's conformance.
- This design maintains the coupling of "value-oriented" and
  "container-oriented" comparison.

The language's restrictions on protocol existentials are severe, and we ought
to address them. I'm not sure that fixing those limitations is enough to
provide an ideal solution to the problems heterogeneous collections face.

There are likely other solutions to these problems too. I'd like to see us
explore this design space and address the problems before we lock ourselves out
of API design changes in Swift 3.
