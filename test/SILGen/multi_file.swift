// RUN: %swift -emit-silgen -primary-file %s %S/Inputs/multi_file_helper.swift | FileCheck %s

// CHECK-LABEL: sil @_TF10multi_file12rdar16016713
func rdar16016713(r: Range) {
  // CHECK: [[LIMIT:%[0-9]+]] = function_ref @_TFV10multi_file5Rangeg5limitSi : $@cc(method) @thin (Range) -> Int
  // CHECK: {{%[0-9]+}} = apply [[LIMIT]]({{%[0-9]+}}) : $@cc(method) @thin (Range) -> Int
  println(r.limit)
}
