// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name trivial_tape %S/../../validation-test/closure_specialization/adcs_trivial_tape.swift -Xllvm -sil-print-around=autodiff-closure-specialization -o %t.sil 2> %t.log
// RUN: %FileCheck %s --check-prefix=BEFORE < %t.log
// RUN: %FileCheck %s --check-prefix=AFTER < %t.log
// RUN: %FileCheck %s --check-prefix=FINAL < %t.sil

// Compile the exact validation source, retaining its actual tuple-result tape
// and owned entry argument. The original tape is nontrivial and destroyed.
// BEFORE-LABEL: // pullback of tupleBranch(_:_:)
// BEFORE: bb0(%{{[0-9]+}} : $Float, %{{[0-9]+}} : $Float, %[[TAPE:[0-9]+]] : @owned $_AD__{{.*}}):
// BEFORE-NEXT: destroy_value %[[TAPE]]
// BEFORE: return
// BEFORE-NEXT: } // end sil function

// The same pullback after specialization has a trivial tape argument. Its
// cleanup is removed locally; all other body instructions still clone normally.
// AFTER-LABEL: // specialized pullback of tupleBranch(_:_:)
// AFTER: // specialized pullback of tupleBranch(_:_:)
// AFTER: bb0(%{{[0-9]+}} : $Float, %{{[0-9]+}} : $Float, %{{[0-9]+}} : $_AD__{{.*}}_spec{{.*}}):
// AFTER-NOT: destroy_value
// AFTER: return
// AFTER-NEXT: } // end sil function

// Ordinary signature optimization can then remove the unused trivial tape.
// FINAL-LABEL: // specialized pullback of tupleBranch(_:_:)
// FINAL: sil private [signature_optimized_thunk] [heuristic_always_inline] {{.*}} : $@convention(thin) (Float, Float) -> (Float, Float) {
// FINAL-NOT: destroy_value
// FINAL: return
// FINAL-NEXT: } // end sil function
