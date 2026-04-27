# Add @preconcurrency import (AddPreconcurrencyImport)

## Overview

Adding `@preconcurrency` to an import suppresses or downgrades `Sendable`-related diagnostics that originate from the imported module.

When a module was written before Swift concurrency was introduced, its types may not have `Sendable` conformances. Importing such a module with `@preconcurrency` tells the compiler to treat missing `Sendable` conformances from that module more leniently, suppressing warnings in `minimal` and `targeted` strict concurrency modes and downgrading errors to warnings in `complete` mode.

To resolve this warning, add `@preconcurrency` to the import declaration:

```swift
@preconcurrency import NonStrictModule
```
