// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil -D DEFINITIONS_ONLY -module-name CallerScan -Xllvm -sil-print-before=autodiff-closure-specialization -Xllvm '-sil-print-function=$s10CallerScan19repeatedBranchCallsyS2f_SbtFTJrSUpSr' %S/../../validation-test/closure_specialization/adcs_caller_scan.swift -o %t.sil 2> %t.log
// RUN: %FileCheck %s < %t.log

// Reusing the caller classification must preserve each branching callee until
// it has had its own opportunity for branch-tape closure specialization.
// CHECK: *** SIL function before {{.*}}stage HighLevel,Module+StackPromote, pass {{[0-9]+}}: AutodiffClosureSpecialization
// CHECK-NEXT: // reverse-mode derivative of repeatedBranchCalls(_:_:)
// CHECK: // function_ref reverse-mode derivative of branchingStep(_:_:)
// CHECK-NEXT: %[[STEP:[0-9]+]] = function_ref
// CHECK-COUNT-3: = apply %[[STEP]](
// CHECK-NOT: = apply %[[STEP]](
// CHECK: } // end sil function
