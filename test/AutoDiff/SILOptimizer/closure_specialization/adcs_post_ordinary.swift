// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -module-name out %S/../../validation-test/closure_specialization/multi_bb_bte.swift -Xllvm -sil-print-around=autodiff-closure-specialization -Xllvm -sil-print-functions=cond_tuple_var -o %t.sil 2> %t.passes
// RUN: %FileCheck %s < %t.passes

// The original pullback captures both the branch tape and individual callbacks.
// CHECK: *** SIL function before {{.*}}#[[VISIT:[0-9]+]], stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// CHECK-NEXT: // reverse-mode derivative of cond_tuple_var
// CHECK: // function_ref pullback of cond_tuple_var
// CHECK-NEXT: %[[ORIGINAL:[0-9]+]] = function_ref {{.*}} : $@convention(thin) (Float, @owned [[OLD_TAPE:_AD__[^, ]+]], @owned @callee_guaranteed
// CHECK-NEXT: {{.*}} = partial_apply {{.*}} %[[ORIGINAL]](%{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}}, %{{[0-9]+}})

// Ordinary specialization removes the separately captured callbacks. In the
// same visit, ADCS rediscovers that pullback and specializes its remaining tape.
// CHECK: *** New SIL function in {{.*}}#[[VISIT]], stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}Function: [[ORDINARY:[^ ]+]]
// CHECK: sil {{.*}}@[[ORDINARY]] : $@convention(thin) (Float, @owned [[OLD_TAPE]]) -> Float
// CHECK: *** New SIL function in {{.*}}#[[VISIT]], stage HighLevel,Module+StackPromote, pass: AutodiffClosureSpecialization {{.*}}Function: [[ORDINARY]][[SUFFIX:[^ ]+]]
// CHECK: sil {{.*}}@[[ORDINARY]][[SUFFIX]] : $@convention(thin) (Float, @owned [[NEW_TAPE:_AD__[^ )]+_spec[^ )]+]]) -> Float
// CHECK: *** SIL function after {{.*}}#[[VISIT]], stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// CHECK-NEXT: // reverse-mode derivative of cond_tuple_var
// CHECK: %[[NEW_PB:[0-9]+]] = function_ref @[[ORDINARY]][[SUFFIX]]
// CHECK-NEXT: %[[BOUND_PB:[0-9]+]] = partial_apply {{.*}} %[[NEW_PB]](%{{[0-9]+}}) : $@convention(thin) (Float, @owned [[NEW_TAPE]]) -> Float
// CHECK-NEXT: %[[RESULT:[0-9]+]] = tuple (%{{[0-9]+}}, %[[BOUND_PB]])
// CHECK-NEXT: return %[[RESULT]]
