# Useless availability check (UselessAvailabilityCheck)

Warnings that identify `if #available` queries that are guaranteed to succeed because an enclosing scope already restricts availability.

## Overview

When an `if #available` query checks for an availability that is already implied by the availability of an enclosing scope (such as a containing declaration's `@available` attribute or another `if #available` block), the check is unnecessary and the guard will always succeed at runtime.

```
@available(macOS 26.4, *)
struct WeatherView: View {
  var body: some View {
    if #available(macOS 26.0, *) { // warning: unnecessary check for 'macOS'; enclosing scope ensures guard will always be true
      // ...
    }
  }
}
```

The `if #available` check above can be removed because `WeatherView` is already restricted to `macOS 26.4` or newer, which implies `macOS 26.0` or newer.
