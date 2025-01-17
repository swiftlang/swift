// RUN: %target-swift-emit-irgen %s -I %S/Inputs -cxx-interoperability-mode=default -Xcc -fno-exceptions -Xcc -fno-objc-exceptions | %FileCheck %s
// REQUIRES: objc_interop

// UNSUPPORTED: CPU=arm64e

import FunctionsObjC

// CHECK: define linkonce_odr {{.*}}ptr @_ZNK7Derived36__synthesizedBaseCall_virtual_methodEv(ptr {{.*}}%[[THIS:.*]])
// CHECK: %[[THIS_ADDR:.*]] = alloca ptr,
// CHECK: store ptr %[[THIS]], ptr %[[THIS_ADDR]],
// CHECK: %[[THIS1:.*]] = load ptr, ptr %[[THIS_ADDR]],
// CHECK: %[[VTABLE:.*]] = load ptr, ptr %[[THIS1]],
// CHECK: %[[VFN:.*]] = getelementptr inbounds ptr, ptr %[[VTABLE]], i64 0
// CHECK: %[[V0:.*]] = load ptr, ptr %[[VFN]],
// CHECK: %[[CALL:.*]] = call {{.*}}ptr %[[V0]](ptr {{.*}}%[[THIS1]])
// CHECK: ret ptr %[[CALL]]

// CHECK: define linkonce_odr {{.*}}ptr @_ZNK7Derived40__synthesizedBaseCall_non_virtual_methodEv(ptr {{.*}}%[[THIS:.*]])
// CHECK: %[[THIS_ADDR:.*]] = alloca ptr,
// CHECK: store ptr %[[THIS]], ptr %[[THIS_ADDR]],
// CHECK: %[[THIS1:.*]] = load ptr, ptr %[[THIS_ADDR]],
// CHECK: %[[CALL:.*]] = call {{.*}}ptr @_ZNK4Base18non_virtual_methodEv(ptr {{.*}}%[[THIS1]])
// CHECK: ret ptr %[[CALL]]

func testBaseMethodCall() -> C {
  let derived = Derived()
  return derived.non_virtual_method()
}

func testBaseVirtualMethodCall() -> C {
  let derived = Derived()
  return derived.virtual_method()
}
