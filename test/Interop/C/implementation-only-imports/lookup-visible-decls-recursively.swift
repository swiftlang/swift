// RUN: %empty-directory(%t)
// RUN: mkdir %t/use_module_a %t/use_module_b
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_a/UseModuleA.swiftmodule %S/Inputs/use-module-a.swift -I %S/Inputs -enable-cxx-interop
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_b/UseModuleB.swiftmodule %S/Inputs/use-module-b.swift -I %S/Inputs -enable-cxx-interop

// RUN: %target-swift-frontend -typecheck -swift-version 5 -I %t/use_module_a -I %t/use_module_b -I %S/Inputs -enable-cxx-interop %s


// If a decl comes from two modules, one of which is marked as
// @_implementationOnly, Swift may choose the @_implementationOnly source
// and then error out due to the decl being hidden.

// Swift should consider all sources for the decl and recognize that the
// decl is not hidden behind @_implementationOnly in all modules.

// This test, as well as `lookup-visible-decls-recursively-inversed.swift`
// checks that the `getFortyTwo` decl can be found when at least one of the
// modules is not `@_implementationOnly`.

import UseModuleA
@_implementationOnly import UseModuleB

@inlinable
public func callFortyTwo() -> CInt {
    return getFortyTwo()
}
