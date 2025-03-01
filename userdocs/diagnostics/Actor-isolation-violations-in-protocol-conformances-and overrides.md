# Actor isolation violations in protocol conformances and overrides

When adding an `async` method to conform to a protocol or override a method from a superclass, the implementation can specify a different actor isolation than the protocol or superclass. When the method is called, the caller will suspend, and the function will later be resumed in the concurrency domain specified in the implementation. This also applies to `get async` accessors required by protocols.

If the implementation defines a different concurrency region than the protocol or superclass, any parameters or return values would potentially be crossing into (or out of) a different concurrency domain. For non-sendable values this is not safe, and the compiler will prevent it. Non-sendable values can't cross actor boundaries!

For example:

```swift
class MyModel {
    var mutableState = 10
}

protocol ModelLogger {
    func log(model: MyModel) async
}

@MainActor 
class MyModelLogger: ModelLogger {
    func log(model: MyModel) async { // ❌ Non-sendable parameter type 'MyModel' cannot be sent from caller of protocol requirement 'log(model:)' into main actor-isolated implementation
        print("Logging model with value \(model.mutableState)")
        return
    }
}
```
Building with complete concurrency checking will diagnose the potential crossing of `MyModel` from a non-isolated domain into the main-actor isolated domain of the `MyModelLogger` implementation. A caller using the `log(model: MyModel)` method from a non-isolated domain would be sneaking a non-sendable `MyModel` into a main actor isolated domain!

```swift
class Repository {
	let model = MyModel()
	let logger: any ModelLogger = MyModelLogger()

	func performLog() {
		logger.log(model: model) // Non-sendable `model` must not cross an actor boundary here!
	}
}
```

There are a few possible ways to address this issue.

## Make all parameters and results `Sendable`

If all the parameters and result types of the function are `Sendable`, it's safe to send them across any actor boundaries. Having a different isolation in the implemenation is no longer a problem:
```swift
final class MyModel: Sendable {
    let state = 10
}

protocol ModelLogger {
    func log(model: MyModel) async
}

@MainActor 
class MyModelLogger: ModelLogger {
    func log(model: MyModel) async { // ✅
        print("Logging model with value \(model.state)")
        return
    }
}
```
Types can be made `Sendable` by an explicit `Sendable` conformance, a `@MainActor` annotation, or with `@unchecked Sendable` when the type has been made thread safe by a different synchronization mechanism like a lock or a queue.

## Match the isolation of both the protocol and the implementation

Of course, making all parameters and results `Sendable` may not always be possible. Since the problem arises when the protocol or superclass method and its implementation are in different concurrency domains, another solution is to annotate either the protocol or the implementation to be on the same concurrency domain:
```swift
class MyModel: Sendable {
    var mutableState = 10
}

@MainActor 
protocol ModelLogger {
    func log(model: MyModel) async
}

@MainActor 
class MyModelLogger: ModelLogger {
    func log(model: MyModel) async { // ✅
        print("Logging model with value \(model.mutableState)")
        return
    }
}
```
This means any callers of these methods will also need to be on the same concurrency domain. For the example above, calls to `logger.log(model: model)` need to be performed from a main-actor isolated domain.

## Make non-sendable parameters or results `sending`

If the parameters or results can't be made `Sendable`, and the implementation needs to be on a diferent isolation domain than the protocol or superclass, it may still be possible to make those parameters or results `sending`. The compiler will ensure that crossings of a non-sendable value into a different isolation domain using `sending` are safe, by making those values unavailable in their original domain after being sent.
```swift
class MyModel {
    var mutableState = 10
}

protocol ModelLogger {
    func log(model: sending MyModel) async
}

@MainActor 
class MyModelLogger: ModelLogger {
    func log(model: sending MyModel) async { // ✅
        print("Logging model with value \(model.mutableState)")
        return
    }
}
```

