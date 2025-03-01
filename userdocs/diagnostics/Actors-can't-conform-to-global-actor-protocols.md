# Actors can't conform to global actor protocols

Attempting to add a protocol conformance to an actor will emit a compiler error if that protocol requires a global actor isolation. For example:

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

This happens because adding a protocol conformance to a protocol requiring global actor isolation (like `SampleProtocol` above) makes all methods and properties on that type inherit that global actor's isolation. But actor types (like `MyModel`) also isolate all methods and properties to the actor!

This makes actors incompatible with protocols that require a global actor isolation. Consider the `foo()` function in the example above: should it be isolated to the `@MainActor`, or to an instance of `MyModel`? What about `bar()`?

The most common fix is to ensure both the protocol and the implementation are in the same concurrency domain. For example, by applying the same global actor isolation to both, instead of using an actor instance for the latter:
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

Alternatively, you can move the global actor isolation annotation from the protocol requirement as a whole to individual methods, and have the conformance use the same isolation for those methods:
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

If the protocol requirements are all `async` functions with `Sendable` parameters and results, it's also possible to have a conformance in a different concurrency region:
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
