# Using Protocols with `Self` or Associated Type Requirements

Protocols in Swift may be used as types, as part of a generic constraint, or as part of an opaque result type.

```swift
// `CustomStringConvertible` can be used as a type.
func foo(bar: CustomStringConvertible) { /* ... */ }

// ...or as a generic constraint on `T`.
func bar<T: CustomStringConvertible>(baz: T) { /* ... */ }

// ...or as part of an opaque result type.
func baz() -> some CustomStringConvertible { /* ... */ }
```

While all Swift protocols can be used as generic constraints and as part of opaque result types, not all protocols can be used as types. Specifically, if a protocol has a requirement which references `Self` (in contexts other than a function's return type) or an associated type, it cannot be used as a type. For example, the protocol `Equatable` requires `static func == (lhs: Self, rhs: Self) -> Bool`, and the protocol `Identifiable` requires `var id: ID { get }`, where `ID` is an associated type. As a result, the following code is not allowed:

```swift
func foo(bar: Equatable) { /* ... */ }
// error: protocol 'Equatable' can only be used as a generic constraint because it has Self or associated type requirements

func foo(bar: Identifiable) { /* ... */ }
// error: protocol 'Identifiable' can only be used as a generic constraint because it has Self or associated type requirements
```

These `Self` or associated type requirements cannot be used via a protocol type because they do not have a well-defined meaning without a concrete conforming type. Therefore, Swift does not support the use of protocols as types if they have such `Self` or associated type requirements, since those types would be able to present only a potentially unusable subset of the interface required for instances of concrete conforming types.

When working with protocols that have `Self` or associated type requirements, most use cases can be supported by constrained generics, opaque result types, or manual type erasure. To learn more, see the sections on [protocols](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html), [generics](https://docs.swift.org/swift-book/LanguageGuide/Generics.html), and [opaque types](https://docs.swift.org/swift-book/LanguageGuide/OpaqueTypes.html) in _The Swift Programming Language_.
