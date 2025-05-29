// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -I %S/Inputs/inherited-objc-initializers/ -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -I %S/Inputs/inherited-objc-initializers/ -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: objc_interop

import InheritedObjCInits

// CHECK: @objc @_inheritsConvenienceInitializers public class Subclass2 : InheritedObjCInits.HasAvailableInit {
public class Subclass2: HasAvailableInit {
  // CHECK-NEXT: @objc override dynamic public init(unavailable: InheritedObjCInits.UnavailableInSwift)
  // CHECK-NEXT: @objc override dynamic public init()
  // CHECK-NEXT: @objc deinit
} // CHECK-NEXT:{{^}$}}

// CHECK: @objc @_inheritsConvenienceInitializers public class Subclass1 : InheritedObjCInits.HasUnavailableInit {
public class Subclass1: HasUnavailableInit {
  // CHECK-NOT: InheritedObjCInits.UnavailableInSwift
  // CHECK-NEXT: @objc override dynamic public init()
  // CHECK-NEXT: @objc deinit
} // CHECK-NEXT:{{^}$}}
