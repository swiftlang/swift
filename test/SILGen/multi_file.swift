// RUN: %swift -emit-silgen -primary-file %s %S/Inputs/multi_file_helper.swift | FileCheck %s

// CHECK-LABEL: sil @_TF10multi_file12rdar16016713
func rdar16016713(r: Range) {
  // CHECK: [[LIMIT:%[0-9]+]] = function_ref @_TFV10multi_file5Rangeg5limitSi : $@cc(method) @thin (Range) -> Int
  // CHECK: {{%[0-9]+}} = apply [[LIMIT]]({{%[0-9]+}}) : $@cc(method) @thin (Range) -> Int
  println(r.limit)
}

// CHECK-LABEL: sil @_TF10multi_file26lazyPropertiesAreNotStored
func lazyPropertiesAreNotStored(var container: LazyContainer) {
  // CHECK: {{%[0-9]+}} = function_ref @_TFV10multi_file13LazyContainerg7lazyVarSi : $@cc(method) @thin (@inout LazyContainer) -> Int
  println(container.lazyVar)
}

// CHECK-LABEL: sil @_TF10multi_file29lazyRefPropertiesAreNotStored
func lazyRefPropertiesAreNotStored(container: LazyContainerClass) {
  // CHECK: {{%[0-9]+}} = class_method %0 : $LazyContainerClass, #LazyContainerClass.lazyVar!getter.1 : LazyContainerClass -> () -> Int , $@cc(method) @thin (@owned LazyContainerClass) -> Int
  println(container.lazyVar)
}
