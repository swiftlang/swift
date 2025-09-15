// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s \
// RUN:  -enable-objc-interop -module-name attrs_objc

// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name attrs_objc

// RUN: %FileCheck %s --input-file %t.swiftinterface

// REQUIRES: objc_interop

import Foundation

@objcMembers
public class ObjCTest: NSObject {
  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK: @abi(func abiObjCFunc())
  // CHECK: @objc public func abiObjCFunc()
  // CHECK: #else
  // CHECK: @_silgen_name("$s10attrs_objc8ObjCTestC03abiC5CFuncyyF")
  // CHECK: @objc public func abiObjCFunc()
  // CHECK: #endif
  @abi(func abiObjCFunc())
  @objc public func abiObjCFunc() {}

  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK: @abi(func abiImplicitObjCFunc())
  // CHECK: @objc public func abiImplicitObjCFunc()
  // CHECK: #else
  // CHECK: @_silgen_name("$s10attrs_objc8ObjCTestC011abiImplicitC5CFuncyyF")
  // CHECK: @objc public func abiImplicitObjCFunc()
  // CHECK: #endif
  @abi(func abiImplicitObjCFunc())
  public func abiImplicitObjCFunc() {}

  // CHECK: #if {{.*}} $ABIAttributeSE0479
  // CHECK: @abi(func abiIBActionFunc(_: Any))
  // CHECK: @objc @IBAction @_Concurrency.MainActor @preconcurrency public func abiIBActionFunc(_: Any)
  // CHECK: #else
  // CHECK: @_silgen_name("$s10attrs_objc8ObjCTestC15abiIBActionFuncyyypF")
  // CHECK: @objc @IBAction @_Concurrency.MainActor @preconcurrency public func abiIBActionFunc(_: Any)
  // CHECK: #endif
  @abi(func abiIBActionFunc(_: Any))
  @IBAction public func abiIBActionFunc(_: Any) {}
}
