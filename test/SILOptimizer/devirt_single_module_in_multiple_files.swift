// RUN: %target-swiftc_driver -module-name devirt_single_module_in_multiple_files -O  %s %S/Inputs/BaseProblem.swift %S/Inputs/Problems.swift -parse-as-library -Xllvm -sil-disable-pass=inline -emit-sil 2>&1 | %FileCheck %s

public func test() {
  let e = Evaluator()
  e.evaluate(2)
  e.evaluate(1)
}

// CHECK-LABEL: sil private @$s38devirt_single_module_in_multiple_files9EvaluatorCACycfcSiycfU_
// CHECK: %{{.*}} = class_method %{{.*}} : $Problem1, #Problem1.run!1 : (Problem1) -> () -> Int, $@convention(method) (@guaranteed Problem1) -> Int
// CHECK-NEXT: apply
// CHECK: return

// CHECK-LABEL: sil private @$s38devirt_single_module_in_multiple_files9EvaluatorCACycfcSiycfU0_
// CHECK: %{{.*}} = class_method %{{.*}} : $Problem2, #Problem2.run!1 : (Problem2) -> () -> Int, $@convention(method) (@guaranteed Problem2) -> Int
// CHECK-NEXT: apply
// CHECK: return
