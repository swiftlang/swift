# Migrating away from experimental suppressed associated types (OldSuppressedAssociatedTypes)

[SE-503](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0503-suppressed-associated-types.md) 
officially introduced the ability to define protocols with associated types that are `~Copyable` and/or
`~Escapable` (i.e., "suppressed"), which relaxes the requirement on types conforming to that protocol.
During the evolution process, a prototype version of this feature existed under the experimental feature
named `SuppressedAssociatedTypes`. **That experimental feature is now deprecated** and is source-incompatible with the accepted
version detailed in SE-503. 

This document provides high-level guidance for users of this experimental feature to help them
update their code to be compatible with [SE-503](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0503-suppressed-associated-types.md).
Please see the evolution proposal for complete details of the accepted functionality.

## Source Changes

>The difference between the prototype experimental feature and SE-503 is with its treatment of primary associated types that are suppressed.
>
>Thus, if your suppressions were only on associated types that are **not** primary i.e., not mentioned within the angle
brackets of the protocol in which they're defined, then there are no source changes required.

Primary associated types are those that are mentioned within angle brackets of a protocol:

```swift
protocol Mailbox<Items> {
  //            ^~~~~~~ declares that `Items` is a primary associated type
  
  associatedtype Items: ~Copyable
  associatedtype Generator: ~Copyable
  
  // ...
}
```

Relative to the prototype, [SE-503](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0503-suppressed-associated-types.md) 
infers default requirements for primary associated types in places they were not already. The defaults are expanded
within extensions of the protocol:

```swift
extension Mailbox {} // OLD
extension Mailbox where Items: ~Copyable {} // NEW
```

The defaults are also expanded in generic contexts where a conformance requirement involving that protocol is written. 
So, to preserve existing behavior, you'll need add explicit suppressions on the associated types induced by those
conformance requirements within those contexts:

```swift
func foo<T>(..) where T: Mailbox {} // OLD
func foo<T>(..) where T: Mailbox, T.Items: ~Copyable {} // NEW

struct PostOffice<M: Mailbox> {} // OLD
struct PostOffice<M: Mailbox> where M.Items: ~Copyable {} // NEW
```


## ABI Changes

>This section is only of interest to who distribute pre-built shared libraries and declared a public
>protocol that has a suppressed primary associated type.

The implementation of SE-503 changes the mangling scheme for generic signatures as a result of the inferred defaults. 
Suppose you have this protocol:

```swift
protocol Drink<Flavor> {
  associatedtype Flavor: ~Copyable
}
```
and these two functions that are completely explicit about whether `T.Flavor` is Copyable or not:

```swift
func foo<T: Drink>(_ t: T) where T.Flavor: Copyable {}
func bar<T: Drink>(_ t: T) where T.Flavor: ~Copyable {}
```

In the prototype version of the feature, `bar`'s requirement `T.Flavor: ~Copyable` was redundant, whereas 
`foo`'s requirement `T.Flavor: Copyable` was _not_ redundant, so it would be included in the mangled symbol of `foo`:

```
$s1X3fooyyxAA5DrinkRzs8Copyable6FlavorRpzlF ---> X.foo<A where A: X.Drink, A.Flavor: Swift.Copyable>(A) -> ()
$s1X3baryyxAA5DrinkRzlF ---> X.bar<A where A: X.Drink>(A) -> ()
```

With SE-503, this _almost_ flips around. It's now `foo` whose requirement is redundant, and `bar`'s mangled symbol now
mentions the requirement `T.Flavor: ~Copyable`:

```
$s1X3fooyyxAA5DrinkRzlF ---> X.foo<A where A: X.Drink>(A) -> ()
$s1X3baryyxAA5DrinkRz6FlavorRj_zlF ---> X.bar<A where A: X.Drink, A.Flavor: ~Swift.Copyable>(A) -> ()
```

It's possible to preserve the old symbol for `bar` by using `@abi` to produce its symbol as if `T.Flavor: Copyable`:

```swift
// $s1X3baryyxAA5DrinkRzlF ---> X.bar<A where A: X.Drink>(A) -> ()
@abi(func bar<T: Drink>(_ t: T) where T.Flavor: Copyable)
func bar<T: Drink>(_ t: T) where T.Flavor: ~Copyable {}
```

But, only [brittle internal compiler features](https://github.com/swiftlang/swift/blob/main/docs/StandardLibraryProgrammersManual.md#_silgen_name) can be used preserve `foo`'s old mangled symbol `$s1X3fooyyxAA5DrinkRzs8Copyable6FlavorRpzlF`.
