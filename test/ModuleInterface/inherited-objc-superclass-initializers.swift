// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -I %S/Inputs/inherited-objc-initializers/
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -I %S/Inputs/inherited-objc-initializers/
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: objc_interop

import InheritedObjCInits

// CHECK: @objc @_inheritsConvenienceInitializers public class Subclass : InheritedObjCInits.FrameworkObject {
public class Subclass: FrameworkObject {
  // CHECK-NEXT: @objc override dynamic public init(selector: ObjectiveC.Selector)
  // CHECK-NEXT: @objc override dynamic public init(integer: Swift.Int)
  // CHECK-NEXT: @objc override dynamic public init()
  // CHECK-NEXT: @objc deinit
} // CHECK-NEXT:{{^}$}}
