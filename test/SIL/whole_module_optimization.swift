// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -O %s %S/Inputs/whole_module_optimization_helper.swift -o %t.sil -module-name main
// RUN: %FileCheck %s < %t.sil
// RUN: %FileCheck %s -check-prefix=NEGATIVE < %t.sil

// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -O -primary-file %s %S/Inputs/whole_module_optimization_helper.swift -o %t.unopt.sil -module-name main
// RUN: %FileCheck %s -check-prefix=CHECK-SINGLE-FILE < %t.unopt.sil

// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -O -enable-testing %s %S/Inputs/whole_module_optimization_helper.swift -o %t.testing.sil -module-name main
// RUN: %FileCheck %s < %t.testing.sil
// RUN: %FileCheck %s -check-prefix=NEGATIVE-TESTABLE < %t.testing.sil

private func privateFn() -> Int32 {
  return 2
}

// CHECK-LABEL: sil @$s4main9getAnswers5Int32VyF
// CHECK-SINGLE-FILE-LABEL: sil @$s4main9getAnswers5Int32VyF
public func getAnswer() -> Int32 {
  // CHECK: %0 = integer_literal $Builtin.Int32, 42
  // CHECK-NEXT: %1 = struct $Int32 (%0 : $Builtin.Int32)
  // CHECK-NEXT: return %1 : $Int32

  // CHECK-SINGLE-FILE: [[F1:%.*]] = function_ref @$s4main9privateFn33_4704C82F83811927370AA02DFDC75B5ALLs5Int32VyF
  // CHECK-SINGLE-FILE: [[F1T:%.*]] = thin_to_thick_function [[F1]] {{.*}} to $@noescape
  // CHECK-SINGLE-FILE: [[F2:%.*]] = function_ref @$s4main7computeys5Int32VADyXEF
  // CHECK-SINGLE-FILE: [[R:%.*]] = apply [[F2]]([[F1T]])
  // CHECK-SINGLE-FILE: return [[R]] : $Int

  return compute(privateFn)
}
// CHECK: }
// CHECK-SINGLE-FILE: }

// NEGATIVE-NOT: sil {{.+}}privateFn
// NEGATIVE-TESTABLE-NOT: sil {{.+}}privateFn
// NEGATIVE-NOT: sil {{.+}}compute
// CHECK-TESTABLE: sil {{.+}}compute
