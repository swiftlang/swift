# Calling an actor-isolated method from a synchronous nonisolated context

Calls to actor-isolated methods from outside the actor must be done asynchronously. Otherwise, access to actor state can happen concurrently and lead to data races. These rules also apply to global actors like the main actor.

For example:

```swift
@MainActor
class MyModel {
  func update() { ... }
}

func runUpdate(model: MyModel) {
  model.update()
}
```

Building the above code produces an error about calling a `@MainActor`-isolated method from outside the actor:

```
| func runUpdate(model: MyModel) {
|   model.update()
|         `- error: call to main actor-isolated instance method 'update()' in a synchronous nonisolated context
| }
```

The `runUpdate` function doesn't specify any actor isolation, so the default is `nonisolated`, meaning it can be called from anywhere.

To resolve the error, `runUpdate` has to make sure the call to `model.update()` is on the main actor. One way to do that is to propagate the main actor isolation to `runUpdate` itself:

```swift
@MainActor
func runUpdate(model: MyModel) {
  model.update()
}
```

Alternatively, if `runUpdate` truly can be called from arbitrary concurrent contexts, the implementation can kick off a new main actor task to switch back to the main actor before calling `model.update()`:

```swift
func runUpdate(model: MyModel) {
  Task { @MainActor in
    model.update()
  }
}
```
