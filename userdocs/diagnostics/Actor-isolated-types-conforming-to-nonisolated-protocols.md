# Actor isolated types conforming to nonisolated protocols

A conformance to a nonisolated protocol in an actor isolated type will be diagnosed when strict concurrency checking is enabled. For example:
```swift
protocol FruitProvider {
    var favoriteFruit: String { get }
    func getAllFruits() -> [String]
}

@MainActor 
final class MyFruitModel: FruitProvider {
    var favoriteFruit = "apple" // ❌ Main actor-isolated property 'favoriteFruit' cannot be used to satisfy nonisolated requirement from protocol 'FruitProvider'
    
    func getAllFruits() -> [String] { // ❌ Main actor-isolated instance method 'getAllFruits()' cannot be used to satisfy nonisolated requirement from protocol 'FruitProvider'
        return ["apple", "orange", "pineapple"]
    }
}
```
This is to guarantee no code can call actor isolated code from a different concurrency region when using a protocol as a type. For example, in this code:
```swift
nonisolated func useProtocol(fruitProvider: any FruitProvider) async {
    print(fruitProvider.favoriteFruit)
}
```
If the compiler allowed creating a `FruitProvider` implementation with `@MainActor` isolation, the code above would be executing main actor isolated code in a non-isolated region, which would break Swift Concurrency isolation guarantees!

There are multiple ways to address this issue.

## Match the isolation of both the protocol and the implementation

If the protocol should be isolated, adding a global actor annotation like `@MainActor` may be the best solution, either for the entire protocol:
```swift
@MainActor 
protocol FruitProvider {
    var favoriteFruit: String { get }
    func getAllFruits() -> [String]
}
```
Or for specific requirements only:
```swift
protocol FruitProvider {    
    @MainActor
    var favoriteFruit: String { get }
    @MainActor
    func getAllFruits() -> [String]
}
```
Since adding a global actor attribute to the protocol will cause conforming types to inherit that isolation, per-requirement annotations should be favored if it makes sense to have conforming types on a different global actor.

## Make properties and methods `nonisolated` on the conforming type

If the required methods don't need to access any mutable state protected by the actor, another solution is to opt-out of that isolation with an explicit `nonisolated` annotation in the conforming methods:
```swift
@MainActor 
final class MyFruitModel: FruitProvider {
    nonisolated let favoriteFruit = "apple" // ✅
    
    func getAllFruits() -> [String] { // ✅
        return ["apple", "orange", "pineapple"]
    }
}
```

## Make protocol requirements `async`

For `async` methods with `Sendable` parameters and results, the implementation can define a different isolation than the protocol. Since the caller needs to add a suspension point to call the method, the execution can be performed in a different concurrency region, and then return to the caller's concurrency region again.

```swift
protocol FruitProvider {
    var favoriteFruit: String { get async }
    func getAllFruits() async -> [String]
}

@MainActor 
final class MyFruitModel: FruitProvider {
    var favoriteFruit = "apple" // ✅
    
    func getAllFruits() -> [String] { // ✅
        return ["apple", "orange", "pineapple"]
    }
}
```

## Use `@preconcurrency`

You can add a `@preconcurrency` annotation to protocol conformances that require methods or properties to be `nonisolated` to supress the compiler diagnostic. Instead, a **runtime** check will be added to ensure the methods are always called from the isolation required by the conforming implementation.
```swift
@MainActor 
final class MyFruitModel: @preconcurrency FruitProvider {
    var favoriteFruit = "apple" // ✅
    
    func getAllFruits() -> [String] { // ✅
        return ["apple", "orange", "pineapple"]
    }
}
```
This is particulary useful when you don't own the protocol, or when incrementally migrating a codebase to Swift 6, where it may not be practical to refactor all code at once. But take care! There's still a runtime check, so our code from earlier will crash if you provide a main actor isolated implementation (like `MyFruitModel`) and then attempt to use that conformance from outside the main actor.
