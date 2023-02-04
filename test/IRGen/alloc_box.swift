// RUN: %target-swift-frontend -Xllvm -sil-disable-pass=SILGenCleanup -primary-file %s -emit-ir -o - | %FileCheck %s

func f() -> Bool? { return nil }

var gb = false
var gc: () -> () = {}

({
  guard var b = f() else { return }
  let c = { b = true }
  gb = b
  gc = c
})()

// CHECK-LABEL: @"$s9alloc_boxyyXEfU0_"
// CHECK-NOT: call void @swift_setDeallocating
// CHECK: call void @swift_deallocUninitializedObject
// CHECK-NOT: call void @swift_setDeallocating
// CHECK: ret void
