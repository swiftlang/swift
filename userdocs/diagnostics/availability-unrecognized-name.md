# Unrecognized availability domains (AvailabilityUnrecognizedName)

Warnings that identify unrecognized availability domain names in `@available` attributes and `if #available` statements.

## Overview

The `AvailabilityUnrecognizedName` group covers warnings emitted when the availability domain specified in an availability related construct is unrecognized by the compiler:

```
@available(NotAValidPlatform, introduced: 1.0) // warning: cannot find availability domain 'NotAValidPlatform'
public func function() {
  if #available(NotAValidPlatform 2.0, *) { // warning: cannot find availability domain 'NotAValidPlatform'
    // ...
  }
}
```

Availability specifications with unrecognized availability domains in `@available` attributes and `#available` queries are ignored by the compiler.
