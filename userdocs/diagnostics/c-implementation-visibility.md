# C and C++ implementation visibility (CImplementationVisibility)

Warnings about the symbol visibility of `@c @implementation` and
`@cxx @implementation` functions.

## Overview

A function marked `@c @implementation` or `@cxx @implementation` provides the
definition of a function declared in a C or C++ header. The compiler emits it
under the symbol name of that declaration, so C and C++ code calls it directly.

Whether that symbol is exported from the binary depends on both the C or C++
declaration and the Swift declaration. The symbol is hidden, and therefore not
exported, if either of the following is true:

- The C or C++ declaration has hidden visibility. This can come from a
  `visibility("hidden")` attribute on the function or on an enclosing class or
  namespace, or from building with `-Xcc -fvisibility=hidden`.
- The Swift declaration is less than `public` or `package`.

Otherwise, the symbol is exported. A hidden symbol is still visible to other
code linked into the same binary, so private and internal implementations can
be called from C and C++ code anywhere in the module.

When a module is compiled with `-enable-testing`, internal implementations are
exported so that `@testable` clients can call them through the C or C++
declaration. Implementations of hidden C and C++ declarations remain hidden.

Because the header wins over the Swift access level, a `public` or `package`
implementation of a hidden declaration is not exported. The compiler warns
about this mismatch:

```c
// Header.h
__attribute__((visibility("hidden"))) int computeChecksum(const char *data);
```

```swift
@c @implementation
public func computeChecksum(_ data: UnsafePointer<CChar>) -> Int32 { ... }
```

```
warning: global function 'computeChecksum(_:)' is declared 'public' but will not be exported, because C function 'computeChecksum' has hidden visibility
```

If the function should be exported, give the C or C++ declaration default
visibility, for example by removing the `visibility("hidden")` attribute or by
adding `__attribute__((visibility("default")))` when building with
`-fvisibility=hidden`.
