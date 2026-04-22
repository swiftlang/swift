# Reparenting Resilient Protocols

> **Status**: This is an experimental feature available via `-enable-experimental-feature Reparenting`
>
> Be very cautious when using this feature!

## Overview

Suppose you have an existing protocol `Contract`,

```swift
// Existing protocol
public protocol Contract {
  associatedtype ID
  func getID() -> ID
  // ...
}
```

and it has mixed in some notions of identity you'd like to factor out into a new protocol, `Identity`:

```
// New parent protocol (proposed)
public protocol Identity {
  associatedtype ID
  associatedtype Validator
  var id: ID { get }
  func getValidator() -> Validator
}
```

Since all Contract-conforming types already have an equivalent notion of "identity", it would be nice if all types 
already conforming to Contract now also conform to Identity. 
[Library evolution](https://github.com/swiftlang/swift/blob/main/docs/LibraryEvolution.rst#protocols) mode rules out the
ability to have Contract inherit from another protocol in a binary-compatible way, unless it is via reparenting.

To reparent Contract with the new protocol Identity, define an extension of Contract with an inheritance clause 
declaring it is `@reparented` by Identity:

```swift
extension Contract: @reparented Identity { /* ... implementation ... */ }
```

This extension declares that Contract was reparented by Identity and is used to synthesize a "default conformance" for
all types conforming to Contract and were not rebuilt in the new world where Contract inherits from Identity.
The default conformance created for `some Contract : Identity` will be used anytime
a client built prior to the reparenting links against the library. If the client
simply recompiles against the new library, each nominal type conforming to Contract will
automatically have their own conformance to Identity created, as usual.
Thus, a default conformance serves to transition clients to a world in which the protocol inheritance relationship does exist.

Here's a complete example of reparenting Contract with Identity, with numbered points for discussion:

```swift
public protocol Contract: Identity { // (1)
  associatedtype ID
  func getID() -> ID
  // ...
}

@reparentable // (2)
@available(monoidLib 9, *) // (3)
public protocol Identity { // (7)
  associatedtype ID
  associatedtype Validator = DefaultValidator  // (4)
  var id: ID { get }
  func getValidator() -> Validator
}

public struct DefaultValidator {}

extension Contract: @reparented Identity // (7)
  where Validator == DefaultValidator { // (5) & (8)

  public var id: ID { getID() } (6)
  public func getValidator() -> Validator { 
    return DefaultValidator()
  }
}
```

1. The inheritance clause of `Contract` (the child) must still reflect that it inherits from `Identity` (the new parent).
2. The new parent is also a new protocol that will be born with `@reparentable` from the outset. It's not safe to add (or remove) the `@reparentable` attribute on an existing protocol, as it can break ABI and source compatability for all clients that have defined their own protocols inheriting from it.
3. Availability may also be required on the new parent protocol, to match the version that protocol was introduced. Unlike traditional protocol inheritance, it's okay if the new parent protocol is less available than the child protocol.
4. Much like introducing new requirements to a protocol, default implementations of all requirements in the new parent protocol must be made available even _outside_ of the reparented extension, to avoid a source break when clients rebuild. In this case, a default type witness for Validator is provided in the Identity, but a restatement of the associatedtype requirement, with a default, can be put in Contract instead.
5. The reparented extension can only consist of a where clause that contains same-type requirements that bind the associated types a concrete type (here `DefaultValidator`). If kept generic, the associated types must have the same name (here, both are called `ID`, so it's an implicit `ID == ID`). Thus, if Identity used the name `Ident` for its associated type requirement, writing `ID == Ident` in the reparented extension is not currently supported.
6. All other requirements of the new parent must have default implementations within scope of the reparented extension. Thus, if there are methods only conditionally available (i.e., within other extensions constrained by a `where` clause), they cannot be used to implement the requirements.
7. Reparentable protocols can only inherit from marker protocols like `Sendable`.
8. You cannot conditionalize a reparented extension based on conformance requirements, e.g., `where ID: Equatable` is not permitted.

## Gotchas

There are a few subtle aspects of reparenting to keep in mind.

### Overhead

For any type conforming to a reparentable protocol, accessing the witness table for that reparentable protocol is less efficient; a relative offset amounting to an extra subtraction of two addresses must be computed. This has more overhead than a normal protocol, which uses a fixed offset.   

### Default Type Witnesses

In the Identity example, Validator is a new associated type requirement that has a default witness DefaultValidator. Suppose another resilient library downstream of your library has types conforming to Contract. If they rebuild against your library that introduces the new inheritance relationship, _without_ declaring a different type to witness the Validator requirement for all Contract-conforming types, then those types will be forever stuck with DefaultValidator as part of their ABI. 

### Availability

Access to all requirements and associated types of the protocol become gated by availability. So, given `some Contract`, you could not call `getValidator()` or it cast to `some Identity` without making it conditional on availability of the protocol.

### Overload Resolution

If your new parent protocol introduces requirements that have the same name as those mentioned in protocols now-inheriting from it, problems can ensue and you may need to use `@_disfavoredOverload`.

### Overrides

All requirements in an inheritor of a reparentable protocol are implicitly treated as if they were `@_nonoverride`, relative to the reparentable protocol's requirements. In other words, the witness table entries of inheriting protocols are not merged together with identical requirements in a parent protocol, if that parent is declared `@reparentable`. Reparentable protocols must have their own witness table entries for all requirements.
