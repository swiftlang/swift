# Missing module version (ModuleVersionMissing)

Warnings that indicate the compiler cannot resolve an `#if canImport(<ModuleName>, _version: <version>)` directive because the module found was not built with a `-user-module-version` flag.


## Overview

Developers may conditionalize which code is active in a source file based on the module version number of an imported dependency module, like this:

```swift
import Dependency

#if canImport(Dependency, _version: 1.2)
// Use declarations introduced in version 1.2 of Dependency
#else
// ...
#endif
```

A dependency's module version must be established by passing the `-user-module-version` flag when compiling the sources of the dependency the module. If no `-user-module-version` flag was specified when the dependency module was built, then the compiler will warn that it cannot resolve the `#if canImport` directive:

```
warning: cannot find user version number for module 'Dependency'; version number ignored [#ModuleVersionMissing]
```

If this diagnostic is emitted, then the `#if canImport` directive implicitly evaluates true.
