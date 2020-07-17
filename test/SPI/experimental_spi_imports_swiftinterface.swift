/// Test the textual interfaces generated with -experimental-spi-imports.

// RUN: %empty-directory(%t)

/// Generate 3 empty modules.
// RUN: touch %t/empty.swift
// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name ExperimentalImported -emit-module-path %t/ExperimentalImported.swiftmodule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name IOIImported -emit-module-path %t/IOIImported.swiftmodule -swift-version 5 -enable-library-evolution
// RUN: %target-swift-frontend -emit-module %t/empty.swift -module-name SPIImported -emit-module-path %t/SPIImported.swiftmodule -swift-version 5 -enable-library-evolution

/// Test the generated swiftinterface.
// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/main.swiftinterface -emit-private-module-interface-path %t/main.private.swiftinterface -enable-library-evolution -swift-version 5 -I %t -experimental-spi-imports
// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/main.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/main.private.swiftinterface

@_spi(dummy) @_implementationOnly import ExperimentalImported
// CHECK-PUBLIC-NOT: import ExperimentalImported
// CHECK-PRIVATE: @_implementationOnly @_spi{{.*}} import ExperimentalImported

@_implementationOnly import IOIImported
// CHECK-PUBLIC-NOT: IOIImported
// CHECK-PRIVATE-NOT: IOIImported

@_spi(dummy) import SPIImported
// CHECK-PUBLIC: {{^}}import SPIImported
// CHECK-PRIVATE: @_spi{{.*}} import SPIImported
