# `NonisolatedNonsendingByDefault`

Proposed in [SE-0461], this feature changes the behavior of nonisolated async
functions to run on the actor to which the caller is isolated (if any) by 
default, and provides an explicit way to express the execution semantics for
these functions.

This feature was proposed in [SE-0461](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0461-async-function-isolation.md)

* The `@concurrent` attribute specifies that a function must always 
  switch off of an actor to run.
  This is the default behavior without `NonisolatedNonsendingByDefault`.
* The `nonisolated(nonsending)` modifier specifies that a function must always 
  run on the caller's actor.
  This is the default behavior with `NonisolatedNonsendingByDefault`.

[SE-0461]: https://github.com/swiftlang/swift-evolution/blob/main/proposals/0461-async-function-isolation.md
