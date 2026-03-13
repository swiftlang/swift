// RUN: %target-swift-emit-sil -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none | %FileCheck %s

import Subscripts

func testGetX() -> CInt {
    let derived = CopyTrackedDerivedClass(42)
    return derived[0]
}

let _ = testGetX()

// CHECK: sil shared [transparent] @$sSo23CopyTrackedDerivedClassVys5Int32VADcig : $@convention(method) (Int32, @in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK: {{.*}}(%[[INT_VAL:.*]] : $Int32, %[[SELF_VAL:.*]] : $*CopyTrackedDerivedClass):
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_operatorSubscript{{.*}} : $@convention(cxx_method) (Int32, @in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK-NEXT: apply %{{.*}}(%[[INT_VAL]], %[[SELF_VAL]])
