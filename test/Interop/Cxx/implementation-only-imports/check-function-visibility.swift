// RUN: %empty-directory(%t)
// RUN: %target-swiftxx-frontend -emit-module -o %t/FortyTwo.swiftmodule -I %S/Inputs %s

// Swift should consider all sources for a decl and recognize that the
// decl is not hidden behind @_implementationOnly in all modules.

// This test, as well as `check-function-visibility-inversed.swift`
// checks that the `getFortyTwo()` decl can be found when at least one of the
// modules is not `@_implementationOnly`.

@_implementationOnly import UserA
import UserB

@inlinable
public func callFortyTwo() -> CInt {
  return getFortyTwo()
}
