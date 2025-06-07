// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s -module-name Test -target %target-cpu-apple-macos51
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t/Test.swiftinterface

// RUN: %target-swift-emit-module-interface(%t/TestMinInlining.swiftinterface) %s -module-name Test -target %target-cpu-apple-macos51 -target-min-inlining-version min
// RUN: %target-swift-typecheck-module-from-interface(%t/TestMinInlining.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t/TestMinInlining.swiftinterface

// REQUIRES: OS=macosx

// CHECK-LABEL: public struct StructWithProperties
public struct StructWithProperties {
  // CHECK:      @available(macOS, unavailable)
  // CHECK-NEXT: public var unavailableComputed: Swift.Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  @available(macOS, unavailable)
  public var unavailableComputed: Int { 1 }

  // CHECK:      @available(macOS 51, *)
  // CHECK-NEXT: public let introducedAtDeploymentStored: Swift.Int
  @available(macOS 51, *)
  public let introducedAtDeploymentStored: Int = 1

  // CHECK:      @available(macOS 99, *)
  // CHECK-NEXT: public var introducedAfterDeploymentComputed: Swift.Int {
  // CHECK-NEXT:   get
  // CHECK-NEXT: }
  @available(macOS 99, *)
  public var introducedAfterDeploymentComputed: Int { 1 }

  // CHECK:      @available(macOS, unavailable)
  // CHECK-NEXT: public let introducedAtDeploymentSPIStored: Swift.Int
  @_spi_available(macOS 51, *)
  public let introducedAtDeploymentSPIStored: Int = 1
}

// CHECK-LABEL: public enum EnumWithAssociatedValues
public enum EnumWithAssociatedValues {
  // CHECK:      @available(macOS, unavailable)
  // CHECK-NEXT: case unavailable
  @available(macOS, unavailable)
  case unavailable

  // CHECK:      @available(macOS 51, *)
  // CHECK-NEXT: case introducedAtDeployment
  @available(macOS 51, *)
  case introducedAtDeployment

  // CHECK:      @available(macOS 99, *)
  // CHECK-NEXT: case introducedAfterDeployment
  @available(macOS 99, *)
  case introducedAfterDeployment

  // CHECK:      @available(macOS, unavailable)
  // CHECK-NEXT: case unavailableWithAssoc(Swift.Int)
  @available(macOS, unavailable)
  case unavailableWithAssoc(Int)

  // CHECK:      @available(macOS 51, *)
  // CHECK-NEXT: case introducedAtDeploymentWithAssoc(Swift.Int)
  @available(macOS 51, *)
  case introducedAtDeploymentWithAssoc(Int)
}

// CHECK-LABEL: public protocol Proto
public protocol Proto {
  // CHECK:      @available(macOS 99, *)
  // CHECK-NEXT: func reqIntroducedAfterDeployment()
  @available(macOS 99, *)
  func reqIntroducedAfterDeployment()

  // CHECK:      @available(macOS, unavailable)
  // CHECK-NEXT: func reqIntroducedAsSPIAfterDeployment()
  @_spi_available(macOS 99, *)
  func reqIntroducedAsSPIAfterDeployment()
}
