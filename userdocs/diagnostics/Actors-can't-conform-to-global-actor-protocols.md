# Actors can't conform to global actor protocols

Adding a protocol conformance to an actor will emit a compiler error if that protocol requires a global actor isolation. For example:

```swift
@MainActor protocol SampleProtocol {
    func foo(value: Int)
}

actor MyModel: SampleProtocol { // ❌ Actor 'MyModel' cannot conform to global actor isolated protocol 'SampleProtocol'
    func foo(value: Int) {
        // ...
    }
    func bar(value: Int) {
        // ...
    }
}
```

A protocol requiring global actor isolation (like `SampleProtocol` above) makes all methods and properties on conforming types inherit that global actor's isolation. But methods and properties in actor types (like `MyModel`) already inherit that actor's isolation!

This makes actors incompatible with protocols that require a global actor isolation: methods can't be isolated to both the actor instance and the global actor. Consider the `foo()` method in the example above. Should it be isolated to the `@MainActor`, or to an instance of the `MyModel` actor? What about `bar()`?

The most common solution is to ensure both the protocol and the implementation are in the same concurrency domain. For example, by applying the same global actor isolation to both, instead of using an actor instance for the latter:
```swift
@MainActor protocol SampleProtocol {
    func foo()
}

@MainActor final class MyModel: SampleProtocol { // ✅
    func foo(value: Int) {
        // ...
    }
    func bar(value: Int) {
        // ...
    }
}
```

Alternatively, you can move the global actor isolation annotation to individual protocol requirements, and have the implementation use the same isolation for those methods:
```swift
protocol SampleProtocol {
    @MainActor func foo()
}

actor MyModel: SampleProtocol { // ✅
    @MainActor func foo(value: Int) {
        // ...
    }
    func bar(value: Int) {
        // ...
    }
}
```

If the protocol requirements are all `async` functions with `Sendable` parameters and results, it's possible to have an implementation in a different concurrency region:
```swift
protocol SampleProtocol {
    @MainActor func foo(value: Int) async
}

actor MyModel: SampleProtocol { // ✅
    func foo(value: Int) async {
        // ...
    }
    func bar() {
        // ...
    }
}
```
This is possible because `async` methods require a suspension point at the call site. After the caller suspends, the implementation can be resumed on a different concurrency region —as long as all parameters and result types are `Sendable`— and, once completed, suspend again and return to the caller.
