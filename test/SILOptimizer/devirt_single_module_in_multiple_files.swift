// RUN: %target-swiftc_driver -module-name devirt_single_module_in_multiple_files -O  %s %S/Inputs/BaseProblem.swift %S/Inputs/Problems.swift -parse-as-library -emit-sil 2>&1 | FileCheck %s

public func test() {
  let e = Evaluator()
  e.evaluate(2)
  e.evaluate(1)
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTSf3cpfr69_TFFC38devirt_single_module_in_multiple_files9EvaluatorcFT_S0_U_FT_Si_n_n___TTRXFo__dSi_XFo_iT__iSi_
// CHECK: %{{.*}} = class_method %{{.*}} : $Problem1, #Problem1.run!1 : (Problem1) -> () -> Int , $@convention(method) (@guaranteed Problem1) -> Int
// CHECK-NEXT: apply
// CHECK: return

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTSf3cpfr70_TFFC38devirt_single_module_in_multiple_files9EvaluatorcFT_S0_U0_FT_Si_n_n___TTRXFo__dSi_XFo_iT__iSi_
// CHECK: %{{.*}} = class_method %{{.*}} : $Problem2, #Problem2.run!1 : (Problem2) -> () -> Int , $@convention(method) (@guaranteed Problem2) -> Int
// CHECK-NEXT: apply
// CHECK: return
