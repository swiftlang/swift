// RUN: %target-swift-frontend -target arm64e-apple-ios12.0 -parse-as-library %s -emit-ir -module-name test | %FileCheck %s

// REQUIRES: CPU=arm64e
// REQUIRES: OS=ios

// Verify that ObjC class metadata fields (isa, superclass, class_ro) are
// signed with the appropriate ptrauth schemas on arm64e.

import Foundation

class MyClass: NSObject {
  @objc func doSomething() {}
}

class MySubclass: MyClass {
  @objc func doMore() {}
}

// The metaclass isa should be signed with ObjCIsaPointers (key 2, disc 0x6AE1 = 27361).
// CHECK: @"OBJC_METACLASS_$__TtC4test7MyClass" =
// CHECK-SAME: .ptrauth
// CHECK-SAME: i32 2
// CHECK-SAME: i64 27361

// The class metadata isa (metaclass pointer) should be signed with ObjCIsaPointers.
// CHECK: @"$s4test7MyClassCMf" =
// CHECK-SAME: .ptrauth

// The superclass pointer should be signed with ObjCSuperPointers (key 2, disc 0xB5AB = 46507).
// CHECK: @"$s4test10MySubclassCMf" =
// CHECK-SAME: .ptrauth

// The class_ro pointer should be signed with ObjCClassROPointers (key 2, disc 0).
// CHECK: .ptrauth
// CHECK-SAME: i32 2
// CHECK-SAME: i64 0
// CHECK-SAME: section "llvm.ptrauth"
