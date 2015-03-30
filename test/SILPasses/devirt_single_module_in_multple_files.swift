// RUN: %target-swiftc_driver -module-name devirt_single_module_in_multiple_files -O  %s %S/Inputs/BaseProblem.swift %S/Inputs/Problems.swift -parse-as-library -emit-sil 2>&1 | FileCheck %s

public func test() {
  let e = Evaluator()
  e.evaluate(2)
  e.evaluate(1)
}

// CHECK-LABEL: sil shared @_TFFC38devirt_single_module_in_multiple_files9EvaluatorcFMS0_FT_S0_U_FT_Si
// CHECK: %{{.*}} = class_method %{{.*}} : $Problem1, #Problem1.run!1 : Problem1 -> () -> Int , $@cc(method) @thin (@owned Problem1) -> Int
// CHECK-NEXT: apply
// CHECK: return

// CHECK-LABEL: sil shared @_TFFC38devirt_single_module_in_multiple_files9EvaluatorcFMS0_FT_S0_U0_FT_Si 
// CHECK: %{{.*}} = class_method %{{.*}} : $Problem2, #Problem2.run!1 : Problem2 -> () -> Int , $@cc(method) @thin (@owned Problem2) -> Int
// CHECK-NEXT: apply
// CHECK: return
