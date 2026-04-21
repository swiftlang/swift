# OSLog module (OSLog)

The `OSLog` module defines logging facilities that integrate with the Swift compiler. This module is provided by the platform vendor and integrates with the system logging facilities. The Swift `OSLog` module should provide a module-scope variable declaration that specifies which section the logging strings should be emitted into. For example:

```swift
let osLogStringSectionName = "__TEXT,__logstrings"
```

The initializer of `osLogStringSectionName` must be a string literal.