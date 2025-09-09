# Deprecated implementation-only imports (ImplementationOnlyDeprecated)

Warnings that identify `import` declarations with the `@_implementationOnly` attribute.

## Overview

When applied to `import` declarations, the compiler-internal attribute `@_implementationOnly` attempts prevents declarations from the imported module from being exposed in the ABI or public interface of the dependent module. This attribute became deprecated when support for access levels on `import` declarations was introduced with [SE-0409].

One reason `@_implementationOnly import` is deprecated is that it is unsafe when used in modules that are built _without_ [library evolution] enabled. For example, suppose the following code were part of a library named `Foo`:

```swift
// Library `Foo`
@_implementationOnly import ImplementationDetail

public struct Bar {
  internal var x: Baz // defined in ImplementationDetail
}
```

If `Foo` is not compiled with library evolution, then the memory layout of values of `Bar` must be known at compile time in clients of `Foo`. However, the `@_implementationOnly import` of `ImplementationDetail` prevents clients from being able to look up `Baz` which is a type that contributes to the layout of `Foo`. As a result, the layout of `Foo` will be miscalculated resulting in undefined behavior.

[SE-0409]: https://github.com/swiftlang/swift-evolution/blob/main/proposals/0409-access-level-on-imports.md
[library evolution]: https://www.swift.org/blog/library-evolution/
