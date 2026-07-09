# Non-Sendable types and Objective-C interoperability (NonSendableObjCInterop)

## Overview

Actor-isolated methods that are `async` can be exposed to Objective-C using `@objc`. These methods are imported into Objective-C with a completion handler parameter. When Objective-C code calls any of these methods, a detached task is created that calls the `async` Swift method in the actor's concurrency domain and eventually forwards the results to the completion handler.

Since Objective-C does not have knowledge of actor isolation, any types involved in the method's signature —either as parameters or as results— are crossing an actor boundary, and are required to be `Sendable`. Otherwise, the crossing will be diagnosed under complete concurrency checking. This is an error in the Swift 6 language mode.

For example:

```swift
@objc
class Succulent: NSObject {
    var waterLevel = 0
}

actor Nursery: NSObject {
    var storedSucculent = Succulent()

    @objc
    func getSucculent() async -> Succulent { // error: non-Sendable type 'Succulent' returned by actor-isolated '@objc' instance method 'getSucculent()' cannot cross actor boundary
        return storedSucculent
    }

    @objc
    func setSucculent(to succulent: Succulent) async { // error: non-Sendable parameter type 'Succulent' of actor-isolated '@objc' instance method 'setSucculent(to:)' cannot cross actor boundary
        storedSucculent = succulent
    }
}
```

Here, `getSucculent() async` provides a non-Sendable `Succulent` type back to Objective-C, but the Objective-C code receiving the `Succulent` is not part of the actor's concurrency domain, which means that an actor boundary is crossed. Similarly, `setSucculent(to succulent: Succulent) async` is passing a `Succulent` from Objective-C code (which can't be isolated to the `Nursery` actor) into the actor's concurrency domain, which also means an actor boundary is crossed. This is not safe for non-Sendable types.

There are a few possible ways to address this issue.

## Make all parameters and results `Sendable`

If all the parameters and result types of the method are `Sendable`, it's safe to send them across any actor boundaries.

```swift
@objc
final class Succulent: NSObject, Sendable {
    let waterLevel = 0
}

actor Nursery: NSObject {
    var storedSucculent = Succulent()

    @objc func getSucculent() async -> Succulent { // OK
        return storedSucculent
    }

    @objc func setSucculent(to succulent: Succulent) async { // OK
        storedSucculent = succulent
    }
}
```

Types can be made `Sendable` by an explicit `Sendable` conformance, a `@MainActor` annotation, or with `@unchecked Sendable` when the type has been made thread safe by a different synchronization mechanism like a lock or a queue.

## Use a global actor like `@MainActor`

Another solution is to use a global actor (like `@MainActor`) instead of an actor's instance to protect mutable state that needs to be accessed from Objective-C:

```swift
@objc
class Succulent: NSObject {
    var waterLevel = 0
}

@MainActor
final class Nursery: NSObject {
    var storedSucculent = Succulent()

    @objc func getSucculent() async -> Succulent { // OK
        return storedSucculent
    }

    @objc func setSucculent(to succulent: Succulent) async { // OK
        storedSucculent = succulent
    }
}
```

However, on the Objective-C side, care must be taken to only call these methods from the global actor's executor. For the main actor, that means only calling these methods from the main thread.

## See Also

- [Unable to conform to Sendable with a non-Sendable superclass (NonSendableSuperclass)](non-sendable-superclass.md)
- [Getting non-Sendable values out of actors (NonSendableExitingActor)](getting-non-sendable-values-out-of-actors.md)
