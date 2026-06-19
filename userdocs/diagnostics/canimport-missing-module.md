# Missing module for canImport (CanImportMissingModule)

Warnings that indicate a versioned `#if canImport(<ModuleName>, _version: <version>)` or `#if canImport(<ModuleName>, _underlyingVersion: <version>)` directive could not find the named module.


## Overview

Developers may use a versioned `#if canImport` directive to conditionally compile code based on the version of a dependency module:

```swift
#if canImport(Dependency, _version: 1.2)
// Use declarations introduced in version 1.2 of Dependency
#endif
```

A versioned `canImport` check evaluates to `false` if the named module cannot be resolved. The compiler emits a warning in case this case:

```
warning: cannot find module 'Dependency' for canImport check; the directive will evaluate to false [#CanImportMissingModule]
```

If the module is expected to be missing in some configurations, you can suppress the warning by checking whether the module can be imported at all before checking its version:

```swift
#if canImport(Dependency) && canImport(Dependency, _version: 1.2)
// ...
#endif
```
