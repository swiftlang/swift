# Conversion from `@isolated(any)` function type to synchronous function type (ConversionFromIsolatedAnyToSynchronous)

## Overview

A function value with type `@isolated(any)` carries its actor isolation dynamically. Calling such a function from a context that does not share its isolation requires hopping to the function's actor, which can only be done from an asynchronous context. As a result, an `@isolated(any)` function value cannot be converted to a synchronous, non-`@isolated(any)` function type.

```swift
@MainActor func updateUI() { /* ... */ }

func runInBackground(_ work: @escaping @Sendable () -> ()) {
  DispatchQueue.global().async { work() }
}

func schedule(_ work: @escaping @isolated(any) @Sendable () -> ()) {
  runInBackground(work) // warning: converting @isolated(any) function of type '@isolated(any) @Sendable () -> ()' to synchronous function type '@Sendable () -> ()' is not allowed
}

schedule(updateUI)
```

In the example, `updateUI()` would run on a background thread instead of the main actor which could result in a data race.

This conversion is currently diagnosed as a warning that will become an error in a future Swift language mode. To opt in to treating it as an error today, pass `-Werror ConversionFromIsolatedAnyToSynchronous`.

To resolve the diagnostic, convert the function value to an `async` function type and use `await` when invoking the function, or remove the `@isolated(any)` attribute from the source type if it is not needed.
