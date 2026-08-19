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

func testUnnamedParam() -> CInt {
    let derived = CopyTrackedDerivedClass(42)
    return derived.unnamedParam(1)
}

let _ = testUnnamedParam()

// CHECK: sil shared @$sSo23CopyTrackedDerivedClassV12unnamedParamys5Int32VAEF : $@convention(method) (Int32, @in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK: bb0(%[[ARG:.*]] : $Int32, %[[SELF:.*]] : $*CopyTrackedDerivedClass):
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_{{.*}} : $@convention(cxx_method) (Int32, @in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK-NEXT: apply %{{.*}}(%[[ARG]], %{{.*}})

func testMixedNamedParams() -> CInt {
    let derived = CopyTrackedDerivedClass(42)
    return derived.mixedNamedParams(1, 2)
}

let _ = testMixedNamedParams()

// CHECK: sil shared @$sSo23CopyTrackedDerivedClassV16mixedNamedParamsys5Int32VAE_AEtF : $@convention(method) (Int32, Int32, @in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK: bb0(%[[UNNAMED:.*]] : $Int32, %[[Y:.*]] : $Int32, %{{.*}} : $*CopyTrackedDerivedClass):
// CHECK: function_ref @{{.*}}__synthesizedBaseCall_{{.*}} : $@convention(cxx_method) (Int32, Int32, @in_guaranteed CopyTrackedDerivedClass) -> Int32
// CHECK-NEXT: apply %{{.*}}(%[[UNNAMED]], %[[Y]], %{{.*}})
