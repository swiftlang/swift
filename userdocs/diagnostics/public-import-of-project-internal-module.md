# Public imports of project-internal modules (PublicImportOfProjectInternalModule)

## Overview

Modules built with `-library-level ipi` are considered project-internal and therefore are not intended to be redistributed. As a result, modules that _are_ intended for redistribution should not import `-library-level ipi` modules publicly since clients will not be able to resolve references to them.

When a module built with `-library-level api` or `-library-level spi` imports a `-library-level ipi` module in a way that exposes the dependency to clients (a bare `public` import, an explicit `public import`, or an `@_exported import`), the compiler emits a warning. For example:

```swift
// MainLib is built with -library-level api
public import ProjectInternalUtilities
  // |- warning: Project internal module 'ProjectInternalUtilities' cannot be imported publicly because 'MainLib' has '-library-level api'
```

## Clang Modules

Clang modules do not have their own `-library-level` setting. To indicate to the compiler that a Clang module is not intended for redistribution, pass `-ipi-clang-module <module name>` when compiling the Swift modules that import it. Imports of the Clang module will be diagnosed the same way that `-library-level ipi` Swift modules are:

```sh
swiftc -library-level api -ipi-clang-module ProjectInternalCUtilities ...
```

```swift
// In a module built with the flags above:
public import ProjectInternalCUtilities
  // |- warning: Project internal module 'ProjectInternalCUtilities' cannot be imported publicly because 'MainLib' has '-library-level api'
```

## Resolving the warning

If the dependency is purely an implementation detail and is never referenced in `public` or `@inlinable` API, mark the import `internal` (or adopt the upcoming feature `InternalImportsByDefault`, where a bare `import` is already `internal`):

```swift
internal import ProjectInternalUtilities
```

If the importing module is itself not meant to be redistributed, lower its library level to `ipi`. Imports between non-distributable modules are unrestricted because the result is also non-distributable:

```sh
swiftc -library-level ipi ...
```

## See Also

- [SE-0409: Access level on imports][SE-0409]

[SE-0409]: https://github.com/swiftlang/swift-evolution/blob/main/proposals/0409-access-level-on-imports.md
