
// RUN: %target-swift-frontend -module-name multi_file -emit-silgen -enable-sil-ownership -primary-file %s %S/Inputs/multi_file_helper.swift | %FileCheck %s

func markUsed<T>(_ t: T) {}

// CHECK-LABEL: sil hidden @$S10multi_file12rdar16016713{{[_0-9a-zA-Z]*}}F
func rdar16016713(_ r: Range) {
  // CHECK: [[LIMIT:%[0-9]+]] = function_ref @$S10multi_file5RangeV5limitSivg : $@convention(method) (Range) -> Int
  // CHECK: {{%[0-9]+}} = apply [[LIMIT]]({{%[0-9]+}}) : $@convention(method) (Range) -> Int
  markUsed(r.limit)
}

// CHECK-LABEL: sil hidden @$S10multi_file26lazyPropertiesAreNotStored{{[_0-9a-zA-Z]*}}F
func lazyPropertiesAreNotStored(_ container: LazyContainer) {
  var container = container
  // CHECK: {{%[0-9]+}} = function_ref @$S10multi_file13LazyContainerV7lazyVarSivg : $@convention(method) (@inout LazyContainer) -> Int
  markUsed(container.lazyVar)
}

// CHECK-LABEL: sil hidden @$S10multi_file29lazyRefPropertiesAreNotStored{{[_0-9a-zA-Z]*}}F
func lazyRefPropertiesAreNotStored(_ container: LazyContainerClass) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $LazyContainerClass):
  // CHECK:   {{%[0-9]+}} = class_method [[ARG]] : $LazyContainerClass, #LazyContainerClass.lazyVar!getter.1 : (LazyContainerClass) -> () -> Int, $@convention(method) (@guaranteed LazyContainerClass) -> Int
  markUsed(container.lazyVar)
}

// CHECK-LABEL: sil hidden @$S10multi_file25finalVarsAreDevirtualizedyyAA18FinalPropertyClassCF
func finalVarsAreDevirtualized(_ obj: FinalPropertyClass) {
  // CHECK: bb0([[ARG:%.*]] : @guaranteed $FinalPropertyClass):
  // CHECK:   ref_element_addr [[ARG]] : $FinalPropertyClass, #FinalPropertyClass.foo
  markUsed(obj.foo)
  // CHECK: class_method [[ARG]] : $FinalPropertyClass, #FinalPropertyClass.bar!getter.1
  markUsed(obj.bar)
}

// rdar://18448869
// CHECK-LABEL: sil hidden @$S10multi_file34finalVarsDontNeedMaterializeForSetyyAA27ObservingPropertyFinalClassCF
func finalVarsDontNeedMaterializeForSet(_ obj: ObservingPropertyFinalClass) {
  obj.foo += 1
  // CHECK: function_ref @$S10multi_file27ObservingPropertyFinalClassC3fooSivg
  // CHECK: function_ref @$S10multi_file27ObservingPropertyFinalClassC3fooSivs
}

// rdar://18503960
// Ensure that we type-check the materializeForSet accessor from the protocol.
class HasComputedProperty: ProtocolWithProperty {
  var foo: Int {
    get { return 1 }
    set {}
  }
}
// CHECK-LABEL: sil hidden [transparent] @$S10multi_file19HasComputedPropertyC3fooSivm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasComputedProperty) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK-LABEL: sil private [transparent] [thunk] @$S10multi_file19HasComputedPropertyCAA012ProtocolWithE0A2aDP3fooSivmTW : $@convention(witness_method: ProtocolWithProperty) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasComputedProperty) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
