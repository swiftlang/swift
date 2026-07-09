# Non-Sendable values in async conformances and overrides (NonSendableInAsyncConformanceOrOverride)

## Overview

An `async` method that satisfies a protocol requirement (or overrides a superclass method) may be isolated to a different actor than the requirement it fulfills. This is allowed because calling an `async` method suspends the caller, letting the implementation resume in its own concurrency domain. (The same applies to `get async` accessors.)

When the two isolations differ, the method's parameters and results have to cross between them. That crossing is only safe for `Sendable` values, so the compiler rejects it for non-Sendable ones.

For example:

```swift
class Plant {
    var height = 0
}

protocol PlantJournal {
    func record(plant: Plant) async
}

@MainActor
class GardenJournal: PlantJournal {
    func record(plant: Plant) async { // error: non-Sendable parameter type 'Plant' cannot be sent from caller of protocol requirement 'record(plant:)' into main actor-isolated implementation
        print("Recorded plant at \(plant.height) cm tall")
    }
}
```

Here, the `GardenJournal` implementation is isolated to the main actor, while the `PlantJournal` requirement is not. A caller in another concurrency domain that invokes `record(plant:)` would pass a non-Sendable `Plant` into the main-actor-isolated implementation. Since that value can't safely cross, the compiler diagnoses the implementation under complete concurrency checking. This is an error in the Swift 6 language mode.

There are a few possible ways to address this issue.

## Make all parameters and results `Sendable`

If all the parameters and result types of the function are `Sendable`, it's safe to send them across any actor boundaries. Often, using a value type like a `struct` or an `enum` is the simplest choice, because they are implicitly `Sendable`. Having a different isolation in the implementation is no longer a problem:

```swift
struct Plant {
    var height = 0
}

protocol PlantJournal {
    func record(plant: Plant) async
}

@MainActor
class GardenJournal: PlantJournal {
    func record(plant: Plant) async { // OK
        print("Recorded plant at \(plant.height) cm tall")
    }
}
```

Reference types can be made `Sendable` by an explicit `Sendable` conformance, a `@MainActor` annotation, or with `@unchecked Sendable` when the type has been made thread safe by a different synchronization mechanism like a lock or a queue.

## Match the isolation of both the protocol and the implementation

Of course, making all parameters and results `Sendable` may not always be possible. Since the problem arises when the protocol or superclass method and its implementation are in different concurrency domains, another solution is to annotate either the protocol or the implementation to be on the same concurrency domain:

```swift
class Plant {
    var height = 0
}

@MainActor
protocol PlantJournal {
    func record(plant: Plant) async
}

@MainActor
class GardenJournal: PlantJournal {
    func record(plant: Plant) async { // OK
        print("Recorded plant at \(plant.height) cm tall")
    }
}
```

This means any callers of these methods will also need to be on the same concurrency domain. For the example above, calls to `journal.record(plant: plant)` need to be performed from a main-actor-isolated domain.

## Make non-Sendable parameters or results `sending`

If the parameters or results can't be made `Sendable`, and the implementation needs to be on a different concurrency domain than the protocol or superclass, it may still be possible to make those parameters or results `sending`. The compiler will ensure that crossings of a non-Sendable value into a different concurrency domain using `sending` are safe, by making those values unavailable in their original domain after being sent. However, since `sending` has to be declared on the requirement, it's part of the protocol's contract for every conformance, even ones whose isolation already matches and wouldn't otherwise have needed it.

```swift
class Plant {
    var height = 0
}

protocol PlantJournal {
    func record(plant: sending Plant) async
}

@MainActor
class GardenJournal: PlantJournal {
    func record(plant: sending Plant) async { // OK
        print("Recorded plant at \(plant.height) cm tall")
    }
}
```

## See Also

- [Protocol conformances crossing into actor-isolated code (ConformanceIsolation)](conformance-isolation.md)
- [Sending value risks causing data races (SendingRisksDataRace)](sending-risks-data-race.md)
