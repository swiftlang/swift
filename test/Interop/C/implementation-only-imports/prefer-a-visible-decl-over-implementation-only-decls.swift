// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/FortyTwo.swiftmodule -I %S/Inputs %s

// If a decl comes from two modules, one of which is marked as
// @_implementationOnly, Swift may choose the @_implementationOnly source
// and then error out due to the decl being hidden.

// Swift should consider all sources for the decl and recognize that the
// decl is not hidden behind @_implementationOnly in all modules.

// This test, as well as
// `prefer-a-visible-decl-over-implementation-only-decls-inversed.swift`
// checks that the `getFortyTwo` decl can be found when at least one of the
// modules is not `@_implementationOnly`.

@_implementationOnly import UserA
import UserB

@_inlineable
public func callFortyTwo() -> CInt {
    return getFortyTwo()
}
