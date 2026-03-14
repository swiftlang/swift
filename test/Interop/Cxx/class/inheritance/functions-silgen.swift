// RUN: %target-swift-emit-sil -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none | %FileCheck %s

import Functions

func testGetX() -> CInt {
    let derived = CopyTrackedDerivedClass(42)
    return derived.getX()
}

let _ = testGetX()

// CHECK: sil shared @$sSo23CopyTrackedDerivedClassV4getXs5Int32VyF : $@convention(method) (@in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK: {{.*}}(%[[SELF_VAL:.*]] : $*CopyTrackedDerivedClass):
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_{{.*}} : $@convention(cxx_method) (@in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK-NEXT: apply %{{.*}}(%[[SELF_VAL]])
