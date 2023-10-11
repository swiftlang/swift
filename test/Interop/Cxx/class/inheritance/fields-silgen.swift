// RUN: %target-swift-emit-sil -I %S/Inputs -enable-experimental-cxx-interop %s -validate-tbd-against-ir=none | %FileCheck %s

import Fields

func testGetX() -> CInt {
    let derived = CopyTrackedDerivedClass(42)
    return derived.x
}

let _ = testGetX()

// CHECK: sil shared [transparent] @$sSo23CopyTrackedDerivedClassV1xs5Int32Vvg : $@convention(method) (@in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK: {{.*}}(%[[SELF_VAL:.*]] : $*CopyTrackedDerivedClass):
// CHECK: function_ref @{{.*}}__synthesizedBaseGetterAccessor_{{.*}} : $@convention(cxx_method) (@in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK-NEXT: apply %{{.*}}(%[[SELF_VAL]])
