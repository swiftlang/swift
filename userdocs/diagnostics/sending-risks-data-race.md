# Sending value risks causing data races

Sharing mutable state between concurrent tasks can cause data races in your program. Resolve this error by only accessing mutable state in one task at a time.

If a type does not conform to `Sendable` the compiler will enforce that each instance of that type is only accessed by one concurrency domain at a time. The `sending 'x' risks causing data races` error indicates that your code can access a non-`Sendable` value from multiple concurrency domains at once.

For example, if a value can be accessed from the main actor, it's invalid to send the same instance to another concurrency domain while the main actor can still access it. This mistake is common when calling an `async` function on a class from the main actor:

```swift
class Person {
  var name: String = ""
    
  func printNameConcurrently() async {
    print(name)
  }
}

@MainActor
func onMainActor(person: Person) async {
  await person.printNameConcurrently()
}
```

The above code produces:

```
await person.printNameConcurrently()
            |- error: sending 'person' risks causing data races
            `- note: sending main actor-isolated 'person' to nonisolated instance method 'printNameConcurrently()' risks causing data races between nonisolated and main actor-isolated uses
```

This happens because the `printNameConcurrently` function runs off of the main actor, and the `onMainActor` function suspends while waiting for `printNameConcurrently` to complete. While suspended, the main actor can run other tasks that still have access to `person`, which can lead to a data race.

The most common fix is to change the `async` method to run on the caller's actor using the `nonisolated(nonsending)` specifier:

```swift
class Person {
  var name: String = ""
    
  nonisolated(nonsending)
  func printNameConcurrently() async {
    print(name)
  }
}

@MainActor
func onMainActor(person: Person) async {
  await person.printNameConcurrently()
}
```

This eliminates the risk of data-races because `printNameConcurrently` continues to run on the main actor, so all access to `person` is serialized.

You can also enable the `NonisolatedNonsendingByDefault` upcoming feature to make `nonisolated(nonsending)` the default for async functions on non-`Sendable` types.
