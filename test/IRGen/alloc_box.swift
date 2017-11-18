// RUN: %target-swift-frontend -primary-file %s -emit-ir -o - | %FileCheck %s

func f() -> Bool? { return nil }

({
  guard var b = f() else { return }
  let c = { b = true }
  _ = (b, c)
})()

// CHECK-LABEL: @_T09alloc_boxyycfU_
// CHECK: <label>:8:
// CHECK-NOT: call void @swift_setDeallocating
// CHECK: call void @swift_rt_swift_deallocUninitializedObject

