// RUN: %empty-directory(%t)
// RUN: mkdir %t/use_module_a %t/use_module_b
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_a/UseModuleA.swiftmodule %S/Inputs/use-module-a.swift -I %S/Inputs
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_b/UseModuleB.swiftmodule %S/Inputs/use-module-b.swift -I %S/Inputs

// RUN: %target-swift-frontend -typecheck -swift-version 5 -I %t/use_module_a -I %t/use_module_b -I %S/Inputs %s


// If a decl comes from two modules, one of which is marked as
// @_implementationOnly, Swift may choose the @_implementationOnly source
// and then error out due to the decl being hidden.

// Swift should consider all sources for the decl and recognize that the
// decl is not hidden behind @_implementationOnly in all modules.

// This test, as well as `lookup-visible-decls-recursively-inversed.swift`
// ensures that Swift looks into the transitive visible modules as well
// when looking for the `getFortyTwo` decl. 

import UseModuleA
@_implementationOnly import UseModuleB

@inlinable
public func callFortyTwo() -> CInt {
  return getFortyTwo()
}
