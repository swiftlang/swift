// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s \
// RUN:  -enable-objc-interop -module-name attrs_objc

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs_objc

// RUN: %FileCheck %s --input-file %t.swiftinterface \
// RUN:   --implicit-check-not '$ABIAttributeSE0479' --implicit-check-not '@_silgen_name'

// REQUIRES: objc_interop

import Foundation

@objcMembers
public class ObjCTest: NSObject {
  // CHECK: @abi(func abiObjCFunc())
  // CHECK-NEXT: @objc public func abiObjCFunc()
  @abi(func abiObjCFunc())
  @objc public func abiObjCFunc() {}

  // CHECK: @abi(func abiImplicitObjCFunc())
  // CHECK-NEXT: @objc public func abiImplicitObjCFunc()
  @abi(func abiImplicitObjCFunc())
  public func abiImplicitObjCFunc() {}

  // CHECK: @abi(func abiIBActionFunc(_: Any))
  // CHECK-NEXT: @objc @IBAction @_Concurrency::MainActor @preconcurrency public func abiIBActionFunc(_: Any)
  @abi(func abiIBActionFunc(_: Any))
  @IBAction public func abiIBActionFunc(_: Any) {}
}
