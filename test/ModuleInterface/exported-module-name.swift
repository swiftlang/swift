// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/CoreKitClient.swiftinterface) %s -module-name CoreKitClient -I %S/Inputs/exported-module-name-before
// RUN: %FileCheck -implicit-check-not BAD %s < %t/CoreKitClient.swiftinterface

// Test that we can rebuild it even when the "export as" module goes away.
// RUN: %target-swift-typecheck-module-from-interface(%t/CoreKitClient.swiftinterface) -module-name CoreKitClient -I %S/Inputs/exported-module-name-after

// CHECK: import CoreKit
import CoreKit

// CHECK-LABEL: public struct CKThingWrapper : Swift.RawRepresentable {
public struct CKThingWrapper: RawRepresentable {
  public var rawValue: CKThing
  public init(rawValue: CKThing) {
    self.rawValue = rawValue
  }
  // Note that this is CoreKit.CKThing, not ExportAsCoreKit.CKThing
  // CHECK: public typealias RawValue = CoreKit.CKThing
} // CHECK: {{^}$}}
