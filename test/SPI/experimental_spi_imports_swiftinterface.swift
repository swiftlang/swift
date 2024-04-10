/// Test the textual interfaces generated with -experimental-spi-imports.

// RUN: %empty-directory(%t)

/// Generate 3 empty modules.
// RUN: touch %t/empty.swift
// RUN: %target-swift-frontend -emit-module %S/Inputs/ioi_helper.swift -module-name ExperimentalImported -emit-module-path %t/ExperimentalImported.swiftmodule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name IOIImported -emit-module-path %t/IOIImported.swiftmodule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name SPIImported -emit-module-path %t/SPIImported.swiftmodule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name InconsistentlyImported -emit-module-path %t/InconsistentlyImported.swiftmodule -swift-version 5 -enable-library-evolution

/// Test the generated swiftinterface.
// RUN: %target-swift-frontend -typecheck %s %S/Inputs/experimental_spi_imports_inconsistent.swift -emit-module-interface-path %t/main.swiftinterface -emit-private-module-interface-path %t/main.private.swiftinterface -enable-library-evolution -swift-version 5 -I %t -experimental-spi-imports
// RUN: %target-swift-typecheck-module-from-interface(%t/main.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/main.private.swiftinterface) -I %t
// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/main.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/main.private.swiftinterface

/// The flag is rejected in Swift 6.
// RUN: not %target-swift-frontend -typecheck % -swift-version 6 \
// RUN:   -experimental-spi-imports 2>&1 | %FileCheck %s -check-prefix=CHECK-6
// CHECK-6: error: '-experimental-spi-imports' is unsupported in Swift 6, use '@_spiOnly' instead

@_spi(dummy) @_implementationOnly import ExperimentalImported
// CHECK-PUBLIC-NOT: import ExperimentalImported
// CHECK-PRIVATE: @_implementationOnly @_spi{{.*}} import ExperimentalImported

// This is also imported as implementation-only via another source file.
import InconsistentlyImported
// CHECK-PUBLIC: {{^}}import InconsistentlyImported
// CHECK-PRIVATE: {{^}}import InconsistentlyImported

@_implementationOnly import IOIImported
// CHECK-PUBLIC-NOT: IOIImported
// CHECK-PRIVATE-NOT: IOIImported

@_spi(dummy) import SPIImported
// CHECK-PUBLIC: {{^}}import SPIImported
// CHECK-PRIVATE: @_spi{{.*}} import SPIImported

@_spi(X)
extension IOIPublicStruct {
  public func foo() {}
}
// CHECK-PUBLIC-NOT: ExperimentalImported.IOIPublicStruct
// CHECK-PRIVATE: @_spi{{.*}} extension ExperimentalImported.IOIPublicStruct
