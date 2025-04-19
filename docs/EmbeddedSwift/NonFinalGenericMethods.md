# Embedded Swift -- Non-final generic methods

**⚠️ Embedded Swift is experimental. This document might be out of date with latest development.**

**‼️ Use the latest downloadable 'Trunk Development' snapshot from swift.org to use Embedded Swift. Public releases of Swift do not yet support Embedded Swift.**

For an introduction and motivation into Embedded Swift, please see "[A Vision for Embedded Swift](https://github.com/swiftlang/swift-evolution/blob/main/visions/embedded-swift.md)", a Swift Evolution document highlighting the main goals and approaches.

## Background

Embedded Swift relies on monomorphization to achieve its properties like not requiring type metadata. Monomorphization is mandatory specialization of all compiled code -- all function bodies get their concrete types substituted and all generics are "compiled out". This is based on passing type information top-down, i.e. from callers to callees, and specializing callees based on the concrete type provided by the caller.

This type information passing from the caller is crucial. If it cannot happen for some reason, then monomorphization cannot happen. This is why Embedded Swift imposes restrictions on non-final generic methods on classes.

## Non-final generic methods on classes (for subclassing-based dispatch)

A non-final generic method on a class where the generic type does not come from the class context itself, is disallowed in Embedded Swift. This is because conservatively, the compiler must assume there could be subclasses with the method overridden. Monomorphization of a function call then cannot know the concrete target type. For example:

```swift
class MyClass {
  func write<T>(t: T) { /* implementation */ }
}

let instance: MyClass = ... // could be MyClass, or a subclass
instance.write(t: 42) // ❌
```

Alternatives (which all have different tradeoffs and code structure implications):

**(1) Make the class final (disallow subclassing):**

```swift
final class MyClass {
  func write<T>(t: T) { /* implementation */ }
}

let instance: MyClass = ... // can only be MyClass
instance.write(t: 42) // ✅
```

**(2) Make the individual method final (disallow overriding in subclasses):**

```swift
class MyClass {
  final func write<T>(t: T) { /* implementation */ }
}

let instance: MyClass = ... // could be MyClass, or a subclass
instance.write(t: 42) // ✅
```

**(3) Make the class generic instead of the method:**

```swift
class MyClass<T> {
  func write(t: T) { /* implementation */ }
}

let instance: MyClass = ... // can only be MyClass<Int>
instance.write(t: 42) // ✅
```

**(4) Use overloading to support a set of concrete types:**

```swift
class MyClass {
  func write(t: Int) { /* implementation */ }
  func write(t: Double) { /* implementation */ }
}

let instance: MyClass = ... // could be MyClass, or a subclass
instance.write(t: 42) // ✅
```

## Non-final generic methods on classes (for existential-based dispatch)

A similar restriction applies to using class-bound existentials for dispatch method calls. Because at compile-time the target type is not statically known, monomorphization is not possible. For example:

```swift
protocol MyProtocol: AnyObject {
    func write<T>(t: T)
}

// existential ("any") is a runtime type-erasing box, we cannot specialize the target
// function for T == Int.self because we don't know the concrete type of "p"
func usingProtocolAsExistential(p: any MyProtocol) {
    p.write(t: 42) // ❌
}
```

Alternatives:

**(1) Avoid using an existential, use generics instead**

```swift
protocol MyProtocol: AnyObject {
    func write<T>(t: T)
}

func usingProtocolAsGeneric(p: some MyProtocol) {
    p.write(t: 42) // ✅
}
```

**(2) Use a primary associated type**

```swift
protocol MyProtocol<T>: AnyObject {
    associatedtype T
    func write(t: T)
}

func usingProtocolAsExistential(p: any MyProtocol<Int>) {
    p.write(t: 42) // ✅
}
```

**(3) Use overloading to support a set of concrete types:**

```swift
protocol MyProtocol: AnyObject {
    func write(t: Int)
    func write(t: Double)
}

func usingProtocolAsExistential(p: any MyProtocol) {
    p.write(t: 42) // ✅
}
```

