# Calling a `@oneway(never)` function with `send` (SentOnewayNeverCall)

## Overview

A function declared `@oneway(never)` is designed to be invoked with `await`, so that the caller waits for the function to complete.

By default, every Void-returning, non-throwing, isolated function can be called with either `send` or `await`: `send` runs the call without waiting for it to complete, and `await` waits for it. Annotating functions is not necessary in most cases.

If a method is explicitly annotated using `@oneway(never)` it signals that callers should not continue before the function has completed, for example because it flushes pending work, or because the caller relies on its effects in the next statement.

Calling a `@oneway(never)` function with `send` from another isolation domain is therefore a warning. The caller would continue before the call has completed, which is not what the function was declared for.

This is a warning rather than an error because waiting for a call can never be enforced completely: a caller can always write `Task { await database.flush() }`. The warning points out that a `send` does not do what the function asks for, and projects which want to enforce it can turn it into an error.

For example:

```swift
actor Database {
  @oneway(never) func flush() {
    // write all pending changes to disk
  }

  func log(_ message: String) {
    // ...
  }
}

func shutDown(database: Database) async {
  send database.flush() // warning: '@oneway(never)' instance method 'flush()' is not intended to be called with 'send', use 'await' instead
  await database.flush() // OK

  send database.log("shutting down") // not marked @oneway(never); calling it with either 'send' or 'await' is permitted
}
```

In an asynchronous context, the warning comes with a fix-it that replaces `send` with `await`.

In a synchronous context, where `await` is not available, move the call into an asynchronous function or a task to wait for it:

```swift
func synchronous(database: Database) {
  send database.flush() // warning: '@oneway(never)' instance method 'flush()' is not intended to be called with 'send', use 'await' instead

  Task {
    await database.flush() // OK
  }
}
```

### Calls within the same isolation domain

A `send` of a `@oneway(never)` function from within its own isolation domain is not diagnosed, such as `send self.flush()` in one of the actor's methods, or a `send` from a function isolated to the same global actor as the callee. Such a call is enqueued on the actor itself, and runs after the current job has completed, which is a deliberate way to defer work rather than a way to avoid waiting for it:

```swift
actor Database {
  @oneway(never) func flush() { /* ... */ }

  func didWrite() {
    send flush() // OK: enqueued on this actor, runs after 'didWrite()'
  }
}
```

The isolation of a closure is not taken into account, so a `send` of a `@oneway(never)` function in a closure is diagnosed, even if the closure is isolated to the callee's actor.

### Which calls are checked

`@oneway(never)` is a property of the declaration, not of the function's type, so the warning is emitted when the called function is known:

* A call of a function or a method declared `@oneway(never)`.
* A call of an override of a `@oneway(never)` method. The override is treated as `@oneway(never)` as well, even if it does not repeat the attribute.
* A call through a `@oneway(never)` protocol requirement. The requirement can be witnessed by a function which is not `@oneway(never)`, and a call through the requirement is still diagnosed.

A call through a function value, like a closure stored in a variable, is not diagnosed:

```swift
func shutDownLater(database: Database) {
  let flush = database.flush
  send flush() // not diagnosed: the call goes through a function value
}
```

### Adopting `@oneway(never)`

Because `@oneway(never)` does not change the type of the function, adding or removing it does not change the ABI of the function, so it can be adopted by existing APIs, including `distributed` functions, without changing how they are called remotely.

Adding it to an existing API makes clients which call the function with `send` get a warning, but does not break their source.

`@oneway(never)` cannot be combined with `@oneway` on the same function, and it cannot be written on a function type.

The warning may be silenced for a declaration, including everything inside it, using the `@diagnose` attribute:

```swift
@diagnose(SentOnewayNeverCall, as: ignored, reason: "the flush may complete after shutdown")
func shutDownQuickly(database: Database) {
  send database.flush() // no warning
}
```

The diagnostics in this group can also be controlled with the `-Werror SentOnewayNeverCall` and `-Wwarning SentOnewayNeverCall` compiler flags, or with `@diagnose(SentOnewayNeverCall, as: error)` and `@diagnose(SentOnewayNeverCall, as: warning)` for a single declaration. `-Werror SentOnewayNeverCall` turns the warning into an error.
