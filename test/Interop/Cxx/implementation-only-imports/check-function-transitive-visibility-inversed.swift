// RUN: %empty-directory(%t)
// RUN: mkdir %t/use_module_a %t/use_module_b
// RUN: %target-swiftxx-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_a/UseModuleA.swiftmodule %S/Inputs/use-module-a.swift -I %S/Inputs
// RUN: %target-swiftxx-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_b/UseModuleB.swiftmodule %S/Inputs/use-module-b.swift -I %S/Inputs

// RUN: %target-swiftxx-frontend -typecheck -swift-version 5 -I %t/use_module_a -I %t/use_module_b -I %S/Inputs %s

// Swift should consider all sources for a decl and recognize that the
// decl is not hidden behind @_implementationOnly in all modules.

// This test, as well as `check-function-transitive-visibility.swift`
// ensures that Swift looks into the transitive visible modules as well
// when looking for the `getFortyTwo()` decl. 

@_implementationOnly import UseModuleA
import UseModuleB

@inlinable
public func callFortyTwo() -> CInt {
  return getFortyTwo()
}
