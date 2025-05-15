# Embedded Swift -- Existentials

**⚠️ Embedded Swift is experimental. This document might be out of date with latest development.**

**‼️ Use the latest downloadable 'Trunk Development' snapshot from swift.org to use Embedded Swift. Public releases of Swift do not yet support Embedded Swift.**

For an introduction and motivation into Embedded Swift, please see "[A Vision for Embedded Swift](https://github.com/swiftlang/swift-evolution/blob/main/visions/embedded-swift.md)", a Swift Evolution document highlighting the main goals and approaches.

## Background

Existentials (also known as "any" types) in Swift are a way to express a type-erased value, where the actual type is not known statically, and at runtime it can be any type that conforms to the specified protocol. Because the possible types can vary in size, the representation of such a value is an "existential container" and the actual represented value is stored either inline (when it fits) or indirectly as a pointer to a heap allocation. There are also multiple concrete representations of the existential container that are optimized for different constraints (e.g. for class-bound existentials, the value does not make sense to ever store inline, so the size of the container is matched to hold exactly one pointer).

Existentials are restricted in Embedded Swift in multiple ways, for multiple reasons:

- Value existentials are not allowed. This prevents the optimization barriers and heap allocation indirections that come with those existentials in regular Swift.
- Class-bound protocols can be used as an existential. This still circumvents the often undesired behavior of existentials where they allocate (and deallocate) storage on the heap for the inner value if it cannot fit in the inline buffer, because class references are always refcounted and references are shared.
- Unbounded generic methods cannot be called through an existential.

## Class-bound existentials

Embedded Swift allows and supports class-bound existentials:

```swift
procotol ClassBoundProtocol: AnyObject { // ✅, this means any type that wants to conform to ClassBoundProtocol must be a class type
    func foo()
}

class Base: ClassBoundProtocol { ... }
class Derived: Base { ... } // also conforms to ClassBoundProtocol
class Other: ClassBoundProtocol { ... }

let existential: any ClassBoundProtocol = ... // ✅
existential.foo() // ✅
```

Note that protocols that are not class-bound cannot form existentials (in Embedded Swift):

```swift
let existential: any Equatable = ... // ❌

class MyClass: Equatable { ... }
let existential: any Equatable = MyClass // ❌, not enough that the actual type is a class, the protocol itself must be class-bound
```

Class-bound existentials in Embedded Swift allow the "is" and "as!" / "as?" operators:

```swift
let existential: any ClassBoundProtocol = ...
if existential is Base { ... } // ✅
guard let concrete = existential as? Derived else { ... } // ✅
let concrete = existential as! Derived // ✅, and will trap at runtime if a different type is inside the existential
```

## Restrictions on class-bound existentials

Class-bound existentials in Embedded Swift do come with some restrictions compared to class-bound existentials in regular Swift:

- You cannot use an existential to call a unbounded generic method from the protocol. This is described in depth in [Embedded Swift -- Non-final generic methods](NonFinalGenericMethods.md). For example:
```swift
protocol ClassBoundProtocol: AnyObject {
  func foo<T>(t: T)
}

let ex: any ClassBoundProtocol = ... // ✅
ex.foo(t: 42) // ❌
```

- You cannot use an existential composition of a class-bound protocol with a non-class-bound protocol. For example:
```swift
let ex: any ClassBoundProtocol & OtherClassBound = ... // ✅
let ex: any ClassBoundProtocol & Equatable = ... // ❌
```

## Alternatives to existentials

When existentials are not possible (e.g. because you need struct types in an existential), or not desirable (e.g. because the indirection on a class-bound existential causes an observation performance degradation), consider one of the following alternatives (which all have different tradeoffs and code structure implications):

**(1) Avoid using an existential, use generics instead**

```swift
protocol MyProtocol {
    func write<T>(t: T)
}

func usingProtocolAsGeneric(p: some MyProtocol) {
    p.write(t: 42) // ✅
}
```

**(2) If you only need a different type based on compile-time configuration (e.g. mocking for unit testing), use #if and typealiases:**
```swift
#if UNIT_TESTING
typealias HWAccess = MMIOBasedHWAccess
#else
typealias HWAccess = MockHWAccess
#endif

let v = HWAccess()
```

**(3) If you only have a handful of tightly-coupled types that need to participate in an existential, use an enum instead:**
```swift
enum E {
    case type1(Type1)
    case type2(Type2)
    case type3(Type3)
}
```
