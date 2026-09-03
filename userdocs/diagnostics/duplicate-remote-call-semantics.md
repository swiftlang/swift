# Duplicate remote call semantics (DuplicateRemoteCallSemantics)

Warnings about repeating the same remote call semantic on a `distributed` declaration.

## Overview

Remote call semantics are expressed applying the `@remoteCall(...)` to `distributed` functions or computed property. 
Repeating the same attribute has no additional effect and produces a warning:

```swift
distributed actor Greeter {
  @remoteCall(oneway)
  @remoteCall(oneway) // warning: remote call semantic 'oneway' specified more than once has no additional effect
  distributed func thanks() {}
}
```

Remove the redundant attribute to silence the warning:

```swift
distributed actor Greeter {
  @remoteCall(oneway)
  distributed func thanks() {}
}
```
