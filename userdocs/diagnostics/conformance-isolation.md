# Protocol conformances crossing into actor-isolated code

Protocol conformances crossing into actor-isolated code can cause data races in your program. Resolve this error by ensuring access to isolated state is always done within the actor.

When a type conforms to a protocol, any generic code can perform operations on that type through the protocol. If the operations that the type used to satisfy the protocol requirements are actor-isolated, this may result in a diagnostic indicating that the conformance crosses into actor-isolated code. For example:

```swift
protocol P {
  func f()
}

@MainActor
struct MyData: P {
  func f() { }
}
```

This code will produce an error similar to:

```swift
| @MainActor
| struct MyData: P {
|        |- error: conformance of 'MyData' to protocol 'P' crosses into main actor-isolated code and can cause data races
|        |- note: isolate this conformance to the main actor with '@MainActor'
|        |- note: mark all declarations used in the conformance 'nonisolated'
|        `- note: turn data races into runtime errors with '@preconcurrency'
|   func f() { }
|        `- note: main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement
| }
```

There are several options for resolving this error, as indicated by the notes:

* If all of the operations used to satisfy the protocol's requirements are on the same global actor (such as the main actor), the conformance itself can be isolated to that global actor. This allows the conformance to be used inside code running on that actor, but it cannot be used concurrently. A conformance can be isolated to a global actor the same way as anything else in the language, e.g.,
  ```swift
  @MainActor
  struct MyData: @MainActor P {
    func f() { }
  }
  ```

* If the conformance needs to be usable anywhere, then each of the operations used to satisfy its requirements must be marked `nonisolated`. This means that they will not have access to any actor-specific operations or state, because these operations can be called concurrently from anywhere. The result would look like this:
  ```swift
  @MainActor
  struct MyData: P {
    nonisolated func f() { }
  }
  ```

* If the protocol requirements themselves are meant to always be used from the correct isolation domain (for example, the main actor) but the protocol itself did not describe that requirement, the conformance can be marked with `@preconcurrency`. This approach moves isolation checking into a run-time assertion, which will produce a fatal error if an operation is called without already being on the right actor. A `@preconcurrency` conformance can be written as follows:
  ```swift
  @MainActor
  struct MyData: @preconcurrency P {
    func f() { }
  }
  ```
