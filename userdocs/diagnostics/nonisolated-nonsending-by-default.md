# nonisolated(nonsending) by Default (NonisolatedNonsendingByDefault)

Runs nonisolated async functions on the caller's actor by default.


## Overview

Prior to this feature, nonisolated async functions *never* run on an actor's executor and instead
switch to global generic executor. The motivation was to prevent unnecessary serialization and
contention for the actor by switching off of the actor to run the nonisolated async function, but
this does  have a number of unfortunate consequences:
1. `nonisolated` is difficult to understand
2. Async functions that run on the caller's actor are difficult to express
3. It's easy to write invalid async APIs
4. It's difficult to write higher-order async APIs

Introduced in Swift 6.2, `-enable-upcoming-feature NonisolatedNonsendingByDefault` changes the
execution semantics of nonisolated async functions to always run on the caller's actor by default. A
new `@concurrent` attribute can be added in order to specify that a function must *always* switch
off of an actor to run.
```swift
struct S: Sendable {
  func performSync() {}

  // `nonisolated(nonsending)` is the default
  func performAsync() async {}

  @concurrent
  func alwaysSwitch() async {}
}

actor MyActor {
  let s: Sendable

  func call() async {
    s.performSync() // runs on actor's executor

    await s.performAsync() // runs on actor's executor

    s.alwaysSwitch() // switches to global generic executor
  }
}
```

A `nonisolated(nonsending)` modifier can also be used prior to enabling this upcoming feature in
order to run on the caller's actor. To achieve the same semantics as above, `S` in this case would
instead be:
```swift
struct S: Sendable {
  func performSync() {}

  nonisolated(nonsending)
  func performAsync() async {}

  // `@concurrent` is the default
  func alwaysSwitch() async {}
}
```


## Migration

```sh
-enable-upcoming-feature NonisolatedNonsendingByDefault:migrate
```

Enabling migration for `NonisolatedNonsendingByDefault` adds fix-its that aim to keep the current
async semantics by adding `@concurrent` to all existing nonisolated async functions and closures.


## See Also

- [SE-0461: Run nonisolated async functions on the caller's actor by default](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0461-async-function-isolation.md)

