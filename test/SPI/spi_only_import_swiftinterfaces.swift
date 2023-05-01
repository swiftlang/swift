/// Test the swiftinterfaces and @_spiOnly.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

/// Generate dependencies
// RUN: %target-swift-frontend -emit-module %S/Inputs/ioi_helper.swift \
// RUN:   -module-name A_SPIOnlyImported -emit-module-path %t/A_SPIOnlyImported.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Empty.swift \
// RUN:   -module-name ConstantSPIOnly -emit-module-path %t/ConstantSPIOnly.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Empty.swift \
// RUN:   -module-name InconsistentIOI -emit-module-path %t/InconsistentIOI.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Empty.swift \
// RUN:   -module-name InconsistentPublic -emit-module-path %t/InconsistentPublic.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Empty.swift \
// RUN:   -module-name IOIImported -emit-module-path %t/IOIImported.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/Empty.swift \
// RUN:   -module-name SPIImported -emit-module-path %t/SPIImported.swiftmodule

/// Test the generated swiftinterface.
// RUN: %target-swift-frontend -typecheck %t/FileA.swift %t/FileB.swift \
// RUN:   -experimental-spi-only-imports \
// RUN:   -swift-version 5 -enable-library-evolution -module-name Main -I %t \
// RUN:   -emit-module-interface-path %t/Main.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/Main.private.swiftinterface
// RUN: %target-swift-typecheck-module-from-interface(%t/Main.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/Main.private.swiftinterface) \
// RUN:   -module-name Main -I %t
// RUN: %FileCheck -check-prefix=CHECK-PUBLIC %s < %t/Main.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/Main.private.swiftinterface

/// Test the case of a library-level=SPI where even the public swiftinterface
/// also has SPI details.
// RUN: %target-swift-frontend -typecheck %t/FileA.swift %t/FileB.swift \
// RUN:   -experimental-spi-only-imports \
// RUN:   -swift-version 5 -enable-library-evolution -module-name SPIMain -I %t \
// RUN:   -emit-module-interface-path %t/SPIMain.swiftinterface \
// RUN:   -emit-private-module-interface-path %t/SPIMain.private.swiftinterface \
// RUN:   -library-level spi
// RUN: %target-swift-typecheck-module-from-interface(%t/SPIMain.swiftinterface) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t/SPIMain.private.swiftinterface) \
// RUN:   -module-name SPIMain -I %t
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/SPIMain.swiftinterface
// RUN: %FileCheck -check-prefix=CHECK-PRIVATE %s < %t/SPIMain.private.swiftinterface

//--- Empty.swift
//--- FileA.swift

/// We don't need or want the flag in the swiftinterface
// CHECK-PUBLIC-NOT: -experimental-spi-only-imports
// CHECK-PRIVATE-NOT: -experimental-spi-only-imports

@_spiOnly @_spi(SomeSPIGroup) import A_SPIOnlyImported
// CHECK-PUBLIC-NOT: A_SPIOnlyImported
// CHECK-PRIVATE: {{^}}@_spiOnly @_spi(SomeSPIGroup) import A_SPIOnlyImported

/// This is also imported as SPI only via FileB.swift
@_spiOnly import ConstantSPIOnly
// CHECK-PUBLIC-NOT: ConstantSPIOnly
// CHECK-PRIVATE: {{^}}@_spiOnly import ConstantSPIOnly

/// This is also imported as SPI only via FileB.swift
@_implementationOnly import InconsistentIOI
// CHECK-PUBLIC-NOT: InconsistentIOI
// CHECK-PRIVATE: {{^}}@_spiOnly import InconsistentIOI

/// This is also imported as SPI only via FileB.swift
import InconsistentPublic
// CHECK-PUBLIC: {{^}}import InconsistentPublic
// CHECK-PRIVATE: {{^}}import InconsistentPublic

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
// CHECK-PUBLIC-NOT: A_SPIOnlyImported.IOIPublicStruct
// CHECK-PRIVATE: @_spi{{.*}} extension A_SPIOnlyImported.IOIPublicStruct

//--- FileB.swift
@_spiOnly import ConstantSPIOnly
@_spiOnly import InconsistentPublic
@_spiOnly import InconsistentIOI
