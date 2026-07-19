# Getting non-Sendable values out of actors (NonSendableExitingActor)

## Overview

When an actor is used to protect a non-Sendable value, the compiler will enforce that the non-Sendable value remains in the actor instance's isolation. Accessing an actor's property of non-Sendable type from a different isolation will be diagnosed when complete concurrency checking is enabled. This is an error in the Swift 6 language mode.

For example:

```swift
class Plant {
    var height = 0
}

actor Terrarium {
    var label = "Yarrow"
    let plant = Plant()
}

nonisolated func growAndReport(in terrarium: Terrarium, by amount: Int) async {
    let label = await terrarium.label // Fine, `label` is Sendable.
    let plant = await terrarium.plant // error: non-Sendable type 'Plant' of property 'plant' cannot exit actor-isolated context
    plant.height += amount
    print("\(label) is now \(plant.height) cm tall")
}
```

Non-Sendable types in actor-isolated properties can't be safely accessed concurrently, even when declared with `let`. This is because a `let` declaration can hold a type with internal mutable state, like `Plant`'s `height` in the above example. Letting a `Plant` reference exit the `Terrarium` instance's isolation would allow concurrent mutation, which is a data race.

A useful pattern when working with actors is to only use `Sendable` types to get information in and out of the actor. You may be able to achieve this by writing the functions directly in the actor:

```swift
class Plant {
    var height = 0
}

actor Terrarium {
    var label = "Yarrow"
    let plant = Plant()

    func growAndReport(by amount: Int) { // amount is an Int, which is Sendable
        plant.height += amount // OK
        print("\(label) is now \(plant.height) cm tall")
    }
}
```

Or, if using a global actor like `@MainActor`, by isolating the functions that access the non-Sendable values to that global actor, so those values never have to leave their isolation:

```swift
class Plant {
    var height = 0
}

@MainActor
final class Terrarium {
    var label = "Yarrow"
    let plant = Plant()
}

@MainActor
func growAndReport(in terrarium: Terrarium, by amount: Int) {
    let plant = terrarium.plant // OK
    plant.height += amount
    print("\(terrarium.label) is now \(plant.height) cm tall")
}
```

By moving entire functions into actor-isolated code, you may also be able to reduce the number of suspension points, or even make the function synchronous. This can result in simpler code, as the compiler guarantees that no other code can modify the actor-protected state while you are running code synchronously on that actor.

Once you're willing to reconsider which isolation `Terrarium` belongs to, as in the `@MainActor` example above, it's worth also considering whether `Terrarium` needs isolation at all. In some cases your type may already be used without being sent, and does not need to be `Sendable` or isolated. The compiler will still ensure callers of `Terrarium` protect it, pushing the problem up a level:

```swift
class Plant {
    var height = 0
}

class Terrarium {
    var label = "Yarrow"
    let plant = Plant()
}

nonisolated func growAndReport(in terrarium: Terrarium, by amount: Int) {
    let label = terrarium.label
    let plant = terrarium.plant
    plant.height += amount
    print("\(label) is now \(plant.height) cm tall")
}
```

## See Also

- [Sending value risks causing data races (SendingRisksDataRace)](sending-risks-data-race.md)
- [Cross-isolation data race (RegionIsolationCrossIsolationDataRace)](region-isolation-cross-isolation-data-race.md)
