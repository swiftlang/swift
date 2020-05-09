// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/CoreKitClient.swiftinterface -module-name CoreKitClient -I %S/Inputs/exported-module-name-before %s
// RUN: %FileCheck -implicit-check-not BAD %s < %t/CoreKitClient.swiftinterface

// Test that we can rebuild it even when the "export as" module goes away.
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/CoreKitClient.swiftmodule -I %S/Inputs/exported-module-name-after %t/CoreKitClient.swiftinterface

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
