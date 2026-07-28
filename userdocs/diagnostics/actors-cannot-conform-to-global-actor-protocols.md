# Actors can't conform to global actor protocols (ActorConformanceToGlobalActorProtocol)

## Overview

Adding a protocol conformance to an actor will emit a compiler error if that protocol requires global actor isolation. For example:

```swift
@MainActor
protocol Exhibit {
    func display()
}

actor Terrarium: Exhibit { // error: actor 'Terrarium' cannot conform to global-actor-isolated protocol 'Exhibit'
    func display() {
        // Terrarium instance isolated
    }
    func water() {
        // Terrarium instance isolated
    }
}
```

A protocol requiring global actor isolation (like `Exhibit` above) makes all methods and properties on conforming types inherit that global actor's isolation. But methods and properties in actor types (like `Terrarium`) already inherit that actor's isolation!

This makes actors incompatible with protocols that require a global actor isolation: methods can't be isolated to both the actor instance and the global actor. Consider the `display()` method in the example above. Should it be isolated to the `@MainActor`, or to an instance of the `Terrarium` actor? What about `water()`?

The most common solution is to ensure both the protocol and the implementation have the same isolation. For example, by applying the same global actor isolation to both, instead of using an actor instance for the latter:

```swift
@MainActor
protocol Exhibit {
    func display()
}

@MainActor
final class Terrarium: Exhibit { // OK
    func display() {
        // MainActor isolated
    }
    func water() {
        // MainActor isolated
    }
}
```

Alternatively, you can move the global actor isolation annotation to individual protocol requirements, and have the implementation use the same isolation for those methods:

```swift
protocol Exhibit {
    @MainActor func display()
}

actor Terrarium: Exhibit { // OK
    @MainActor func display() {
        // MainActor isolated
    }
    func water() {
        // Terrarium instance isolated
    }
}
```

If the protocol requirements are all `async` functions with `Sendable` parameters and results, it's possible to have an implementation with a different isolation:

```swift
protocol Exhibit {
    @MainActor func display() async
}

actor Terrarium: Exhibit { // OK
    func display() async {
        // Terrarium instance isolated
    }
    func water() {
        // Terrarium instance isolated
    }
}
```

This is possible because `async` methods require a suspension point at the call site. After the caller suspends, the implementation can be resumed in a different isolation (the `Terrarium` actor instance) so long as all parameters and result types can be sent between isolations. Once completed, the implementation can suspend again and return to the caller.

## See Also

- [Protocol conformances crossing into actor-isolated code (ConformanceIsolation)](conformance-isolation.md)
- [Isolated conformances (IsolatedConformances)](isolated-conformances.md)
