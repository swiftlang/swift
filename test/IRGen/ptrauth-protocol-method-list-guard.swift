// RUN: %target-swift-frontend -target arm64e-apple-ios12.0 -parse-as-library %s -emit-ir -module-name test | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

// Verify that ObjC protocol method list pointers are NOT signed for protocols.
//
// Crash pattern from D93951803 (Crash 12): The ObjC runtime's fixupProtocol
// loads protocol method list pointers with plain ldr (no auth instruction),
// but fixupMethodList authenticates class method lists with autda.
// If Swift IRGen signs protocol method list pointers, the runtime crashes
// with EXC_BAD_ACCESS when dereferencing the PAC-corrupted pointer.
//
// The fix adds a !isBuildingProtocol() guard to the method list pointer
// signing code path. Class method lists ARE signed; protocol method lists
// are NOT.

import Foundation

@objc protocol TestProtocol {
  func protocolMethod()
}

class TestClass: NSObject, TestProtocol {
  @objc func protocolMethod() {}
  @objc func classMethod() {}
}

// Protocol metadata should NOT have signed method list pointers.
// The _PROTOCOL_ metadata struct stores method list pointers without ptrauth.
//
// Class metadata SHOULD have signed method list pointers on arm64e.
// CHECK: @"$s4test9TestClassCMf"
// CHECK-SAME: .ptrauth

// Protocol metadata must NOT have .ptrauth on method list pointers.
// The ObjC runtime's fixupProtocol loads them with plain ldr (no auth).
// CHECK: @"_PROTOCOL__TtP4test12TestProtocol_"
// CHECK-NOT: .ptrauth
