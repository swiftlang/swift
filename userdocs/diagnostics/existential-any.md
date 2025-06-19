# Existential any (ExistentialAny)

`any` existential type syntax.


## Overview

`any` was introduced in Swift 5.6 to explicitly mark "existential types", i.e., abstract boxed types
that conform to a set of constraints. For source compatibility, these are not diagnosed by default
except for existential types constrained to protocols with `Self` or associated type requirements
(as this was introduced in the same version):
```swift
protocol Foo {
  associatedtype Bar

  func foo(_: Bar)
}

protocol Baz {}

func pass(foo: Foo) {} // `any Foo` is required instead of `Foo`

func pass(baz: Baz) {} // no warning or error by default for source compatibility
```

When enabled via `-enable-upcoming-feature ExistentialAny`, the upcoming language feature
`ExistentialAny` will diagnose *all* existential types without `any`:
```swift
func pass(baz: Baz) {} // `any Baz` required instead of `Baz`
```

This will become the default in a future language mode.


## Migration

```sh
-enable-upcoming-feature ExistentialAny:migrate
```

Enabling migration for `ExistentialAny` adds fix-its that prepend all existential types with `any`
as required. No attempt is made to convert to generic (`some`) types.


## See Also

- [SE-0335: Introduce existential `any`](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0335-existential-any.md)
