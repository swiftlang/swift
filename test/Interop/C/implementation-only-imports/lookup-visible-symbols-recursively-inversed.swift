// RUN: %empty-directory(%t)
// RUN: mkdir %t/use_module_a %t/use_module_b
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_a/UseModuleA.swiftmodule %S/Inputs/use-module-a.swift -I %S/Inputs -enable-cxx-interop
// RUN: %target-swift-frontend -enable-library-evolution -swift-version 5 -emit-module -o %t/use_module_b/UseModuleB.swiftmodule %S/Inputs/use-module-b.swift -I %S/Inputs -enable-cxx-interop

// RUN: %target-swift-frontend -typecheck -swift-version 5 -I %t/use_module_a -I %t/use_module_b -I %S/Inputs -enable-cxx-interop %s


// If a symbol comes from two modules, one of which is marked as
// @_implementationOnly, Swift may choose the @_implementationOnly source
// and then error out due to the symbol being hidden.

// Swift should consider all sources for the symbol and recognize that the
// symbol is not hidden behind @_implementationOnly in all modules.

// This test, as well as the `lookup-visible-symbols-recursively.swift` check
// that the `getFortyTwo` symbol can be found when at least one of the
// modules is not `@_implementationOnly`.

@_implementationOnly import UseModuleA
import UseModuleB

@inlinable
public func callFortyTwo() -> CInt {
    return getFortyTwo()
}
