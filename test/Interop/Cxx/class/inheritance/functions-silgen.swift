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

func testUnnamedParams(_ derived: DerivedFromUnnamedParams,
                       _ pointer: UnsafeMutablePointer<CInt>) -> CInt {
  var ref: CInt = 0
  return derived.takesUnnamed(1, true, pointer, NonTrivial(), &ref)
}

// The synthesized body forwards the unnamed parameters as local values, rather
// than as references to global variables.
// CHECK-LABEL: sil shared @$sSo24DerivedFromUnnamedParamsV05takesC0ys5Int32VAE_SbSpyAEGSgSo10NonTrivialVAEztF : $@convention(method)
// CHECK:       bb0(%[[I:[0-9]+]] : $Int32, %[[B:[0-9]+]] : $Bool, %[[P:[0-9]+]] : $Optional<UnsafeMutablePointer<Int32>>, %[[NT:[0-9]+]] : $*NonTrivial, %[[R:[0-9]+]] : $*Int32, %[[SELF:[0-9]+]] : $*DerivedFromUnnamedParams):
// CHECK-NOT:   global_addr
// CHECK:       %[[ACCESS:[0-9]+]] = begin_access [modify] [static] %[[R]]
// CHECK:       apply %{{[0-9]+}}(%[[I]], %[[B]], %[[P]], %[[NT]], %[[ACCESS]], %[[SELF]])
// CHECK:       } // end sil function '$sSo24DerivedFromUnnamedParamsV05takesC0ys5Int32VAE_SbSpyAEGSgSo10NonTrivialVAEztF'
