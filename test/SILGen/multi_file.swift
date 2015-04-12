// RUN: %target-swift-frontend -emit-silgen -primary-file %s %S/Inputs/multi_file_helper.swift | FileCheck %s

// CHECK-LABEL: sil hidden @_TF10multi_file12rdar16016713
func rdar16016713(r: Range) {
  // CHECK: [[LIMIT:%[0-9]+]] = function_ref @_TFV10multi_file5Rangeg5limitSi : $@cc(method) @thin (Range) -> Int
  // CHECK: {{%[0-9]+}} = apply [[LIMIT]]({{%[0-9]+}}) : $@cc(method) @thin (Range) -> Int
  println(r.limit)
}

// CHECK-LABEL: sil hidden @_TF10multi_file26lazyPropertiesAreNotStored
func lazyPropertiesAreNotStored(var container: LazyContainer) {
  // CHECK: {{%[0-9]+}} = function_ref @_TFV10multi_file13LazyContainerg7lazyVarSi : $@cc(method) @thin (@inout LazyContainer) -> Int
  println(container.lazyVar)
}

// CHECK-LABEL: sil hidden @_TF10multi_file29lazyRefPropertiesAreNotStored
func lazyRefPropertiesAreNotStored(container: LazyContainerClass) {
  // CHECK: {{%[0-9]+}} = class_method %0 : $LazyContainerClass, #LazyContainerClass.lazyVar!getter.1 : LazyContainerClass -> () -> Int , $@cc(method) @thin (@guaranteed LazyContainerClass) -> Int
  println(container.lazyVar)
}

// CHECK-LABEL: sil hidden @_TF10multi_file25finalVarsAreDevirtualizedFCS_18FinalPropertyClassT_
func finalVarsAreDevirtualized(obj: FinalPropertyClass) {
  // CHECK: ref_element_addr %0 : $FinalPropertyClass, #FinalPropertyClass.foo
  println(obj.foo)
  // CHECK: class_method %0 : $FinalPropertyClass, #FinalPropertyClass.bar!getter.1
  println(obj.bar)
}

// rdar://18448869
// CHECK-LABEL: sil hidden @_TF10multi_file34finalVarsDontNeedMaterializeForSetFCS_27ObservingPropertyFinalClassT_
func finalVarsDontNeedMaterializeForSet(obj: ObservingPropertyFinalClass) {
  obj.foo++
  // CHECK: function_ref @_TFC10multi_file27ObservingPropertyFinalClassg3fooSi
  // CHECK: function_ref @_TFC10multi_file27ObservingPropertyFinalClasss3fooSi
}

// rdar://18503960
// Ensure that we type-check the materializeForSet accessor from the protocol.
class HasComputedProperty: ProtocolWithProperty {
  var foo: Int {
    get { return 1 }
    set {}
  }
}
// CHECK-LABEL: sil hidden [transparent] @_TFC10multi_file19HasComputedPropertym3fooSi : $@cc(method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasComputedProperty) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasComputedProperty, @thick HasComputedProperty.Type) -> ()>) {
// CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWC10multi_file19HasComputedPropertyS_20ProtocolWithPropertyS_FS1_m3fooSi : $@cc(witness_method) @thin (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasComputedProperty) -> (Builtin.RawPointer, Optional<@thin (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout HasComputedProperty, @thick HasComputedProperty.Type) -> ()>) {
