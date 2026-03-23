// Test that extract-module-imports reads a .swiftmodule and emits import lines
// for all supported import kinds.
//
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build dependency modules that TestMod will import.
// RUN: %target-swift-frontend -emit-module -module-name DepDefault -o %t %t/DepDefault.swift
// RUN: %target-swift-frontend -emit-module -module-name DepExported -o %t %t/DepExported.swift
// RUN: %target-swift-frontend -emit-module -module-name DepImplOnly -o %t %t/DepImplOnly.swift
// RUN: %target-swift-frontend -emit-module -module-name DepPackage -o %t %t/DepPackage.swift
// RUN: %target-swift-frontend -emit-module -module-name DepInternal -o %t %t/DepInternal.swift
// RUN: %target-swift-frontend -emit-module -module-name DepSPI -o %t %t/DepSPI.swift
// RUN: %target-swift-frontend -emit-module -module-name DepSPIOnly -o %t %t/DepSPIOnly.swift

// Build the main module that uses all import kinds.
// RUN: %target-swift-frontend -emit-module -module-name TestMod \
// RUN:   -package-name TestPackage \
// RUN:   -experimental-spi-only-imports \
// RUN:   -I %t -o %t/TestMod.swiftmodule %t/TestMod.swift

// Extract imports from the compiled module.
// RUN: %swift-interface-tool -action extract-module-imports %t/TestMod.swiftmodule > %t/imports.swift
// RUN: %FileCheck %s < %t/imports.swift

// CHECK: // Imports extracted from module: TestMod
// CHECK-DAG: @_exported import DepExported
// CHECK-DAG: import DepDefault
// CHECK-DAG: @_implementationOnly import DepImplOnly
// CHECK-DAG: package import DepPackage
// CHECK-DAG: internal import DepInternal
// CHECK-DAG: @_spi(TestGroup) import DepSPI
// Note: @_spiOnly is serialized as Default by the compiler (merged with
// the Default import set), so it round-trips as a plain import.
// CHECK-DAG: import DepSPIOnly
// CHECK-DAG: import Swift

//--- DepDefault.swift
public func depDefaultFunc() {}

//--- DepExported.swift
public func depExportedFunc() {}

//--- DepImplOnly.swift
public func depImplOnlyFunc() {}

//--- DepPackage.swift
public func depPackageFunc() {}

//--- DepInternal.swift
public func depInternalFunc() {}

//--- DepSPI.swift
@_spi(TestGroup) public func depSPIFunc() {}

//--- DepSPIOnly.swift
@_spi(TestGroup) public func depSPIOnlyFunc() {}

//--- TestMod.swift
import DepDefault
@_exported import DepExported
@_implementationOnly import DepImplOnly
package import DepPackage
internal import DepInternal
@_spi(TestGroup) import DepSPI
@_spiOnly import DepSPIOnly

public func testFunc() {
  depDefaultFunc()
  depExportedFunc()
}
