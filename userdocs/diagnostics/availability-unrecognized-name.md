# Unrecognized availability platforms (AvailabilityUnrecognizedName)

Warnings that identify unrecognized platform names in `@available` attributes and `if #available` statements.

## Overview

The `AvailabilityUnrecognizedName` group covers warnings emitted when the platform name specified in an availability related construct is unrecognized by the compiler:

```
@available(NotAValidPlatform, introduced: 1.0) // warning: unrecognized platform name 'NotAValidPlatform'
public func function() {
  if #available(NotAValidPlatform 2.0, *) { // warning: unrecognized platform name 'NotAValidPlatform'
    // ...
  }
}
```

Availability specifications with unrecognized platform names in `@available` attributes and `#available` queries are ignored by the compiler.
