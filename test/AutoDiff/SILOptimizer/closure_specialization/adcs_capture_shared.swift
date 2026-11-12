// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name adcs_capture_shared %S/../../validation-test/closure_specialization/adcs_capture_shared.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.passes
// RUN: %FileCheck %S/../../validation-test/closure_specialization/adcs_capture_shared.swift < %t.sil
// RUN: %FileCheck %s < %t.passes

// Two tape types share one payload and one indirect capture allocation. ADCS
// removes the producer's stack slot and rematerializes it for the root PAI in
// the pullback. The resulting closure is passed to the direct subset call.
// CHECK-LABEL: // reverse-mode derivative of stringShared(_:_:_:)
// CHECK: %[[OLD_SLOT:[0-9]+]] = alloc_stack $String
// CHECK: %[[OLD_ROOT:[0-9]+]] = partial_apply {{.*}}(%[[OLD_SLOT]])
// CHECK: %[[OLD_SUBSET:[0-9]+]] = partial_apply {{.*}}(%[[OLD_ROOT]])
// CHECK: %[[SHARED:[0-9]+]] = tuple (%[[OLD_SUBSET]], %{{[0-9]+}})
// CHECK: = enum ${{.*}}_bb1__Pred__{{.*}}, %[[SHARED]]
// CHECK: = enum ${{.*}}_bb2__Pred__{{.*}}, %[[SHARED]]
// CHECK-LABEL: // reverse-mode derivative of stringShared(_:_:_:)
// CHECK-NOT: alloc_stack $String
// CHECK-NOT: dealloc_stack
// CHECK: } // end sil function
// CHECK-LABEL: // specialized pullback of stringShared(_:_:_:)
// CHECK: %[[ADAPTER_SLOT:[0-9]+]] = alloc_stack $String
// CHECK-NEXT: store %{{[0-9]+}} to [init] %[[ADAPTER_SLOT]]
// CHECK-NEXT: %[[ADAPTER_ROOT:[0-9]+]] = partial_apply {{.*}}(%[[ADAPTER_SLOT]])
// CHECK-NEXT: dealloc_stack %[[ADAPTER_SLOT]]
// CHECK-NEXT: // function_ref autodiff subset parameters thunk for pullback
// CHECK-NEXT: %[[SUBSET:[0-9]+]] = function_ref
// CHECK-NEXT: {{.*}} = apply %[[SUBSET]](%{{[0-9]+}}, %[[ADAPTER_ROOT]])
// CHECK: destroy_value %[[ADAPTER_ROOT]]
// CHECK: } // end sil function

// ADCS consumes the owned String capture tuple and initializes a stack slot
// for the direct guaranteed-indirect call. The slot is destroyed after use.
// CHECK-LABEL: *** New SIL function in {{.*}}pass: AutodiffClosureSpecialization {{.*}}Function: {{.*}}12stringDirect
// CHECK-NEXT: // specialized pullback of stringDirect
// CHECK: // specialized pullback of stringDirect
// CHECK-NOT: } // end sil function
// CHECK: bb{{[0-9]+}}(%[[PAYLOAD:[0-9]+]] : @owned $(predecessor: {{.*}}, (_: String)))
// CHECK-NEXT: (%{{[0-9]+}}, %[[CAPTURES:[0-9]+]]) = destructure_tuple %[[PAYLOAD]]
// CHECK-NEXT: %[[STRING:[0-9]+]] = destructure_tuple %[[CAPTURES]]
// CHECK-NEXT: %[[SLOT:[0-9]+]] = alloc_stack $String
// CHECK-NEXT: store %[[STRING]] to [init] %[[SLOT]]
// CHECK-NEXT: // function_ref specialized closure #1 in vjpLabeled
// CHECK-NEXT: %[[ROOT:[0-9]+]] = function_ref {{.*}} : $@convention(thin) (Float, @in_guaranteed String) -> (Float, Float)
// CHECK-NEXT: {{%[0-9]+}} = apply %[[ROOT]]({{%[0-9]+}}, %[[SLOT]])
// CHECK-NEXT: destroy_addr %[[SLOT]]
// CHECK-NEXT: dealloc_stack %[[SLOT]]
