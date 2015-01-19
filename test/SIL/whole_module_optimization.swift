// RUN: %target-swift-frontend -emit-sil -O %s %S/Inputs/whole_module_optimization_helper.swift -o %t.sil -module-name main
// RUN: FileCheck %s < %t.sil
// RUN: FileCheck %s -check-prefix=NEGATIVE < %t.sil

// RUN: %target-swift-frontend -emit-sil -O -primary-file %s %S/Inputs/whole_module_optimization_helper.swift -o %t.unopt.sil -module-name main
// RUN: FileCheck %s -check-prefix=CHECK-SINGLE-FILE < %t.unopt.sil

private func privateFn() -> Int {
  return 2
}

// CHECK-LABEL: sil @_TF4main9getAnswerFT_Si
// CHECK-SINGLE-FILE-LABEL: sil @_TF4main9getAnswerFT_Si
public func getAnswer() -> Int {
  // CHECK: %0 = integer_literal $Builtin.Word, 42
  // CHECK-NEXT: %1 = struct $Int (%0 : $Builtin.Word)
  // CHECK-NEXT: return %1 : $Int

  // CHECK-SINGLE-FILE: %0 = function_ref @_TF4main7computeFFT_SiSi
  // CHECK-SINGLE-FILE: %1 = function_ref @_TF4mainP33_4704C82F83811927370AA02DFDC75B5A9privateFnFT_Si
  // CHECK-SINGLE-FILE: %2 = thin_to_thick_function %1
  // CHECK-SINGLE-FILE: %3 = apply %0(%2)
  // CHECK-SINGLE-FILE: return %3 : $Int

  return compute(privateFn)
}
// CHECK: }
// CHECK-SINGLE-FILE: }

// NEGATIVE-NOT: sil {{.+}}privateFn
// NEGATIVE-NOT: sil {{.+}}compute
