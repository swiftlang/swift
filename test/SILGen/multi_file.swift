// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -primary-file %s %S/Inputs/multi_file_helper.swift | %FileCheck %s

func markUsed<T>(_ t: T) {}

// CHECK-LABEL: sil hidden @_T010multi_file12rdar16016713{{[_0-9a-zA-Z]*}}F
func rdar16016713(_ r: Range) {
  // CHECK: [[LIMIT:%[0-9]+]] = function_ref @_T010multi_file5RangeV5limitSifg : $@convention(method) (Range) -> Int
  // CHECK: {{%[0-9]+}} = apply [[LIMIT]]({{%[0-9]+}}) : $@convention(method) (Range) -> Int
  markUsed(r.limit)
}

// CHECK-LABEL: sil hidden @_T010multi_file26lazyPropertiesAreNotStored{{[_0-9a-zA-Z]*}}F
func lazyPropertiesAreNotStored(_ container: LazyContainer) {
  var container = container
  // CHECK: {{%[0-9]+}} = function_ref @_T010multi_file13LazyContainerV7lazyVarSifg : $@convention(method) (@inout LazyContainer) -> Int
  markUsed(container.lazyVar)
}

// CHECK-LABEL: sil hidden @_T010multi_file29lazyRefPropertiesAreNotStored{{[_0-9a-zA-Z]*}}F
func lazyRefPropertiesAreNotStored(_ container: LazyContainerClass) {
  // CHECK: bb0([[ARG:%.*]] : $LazyContainerClass):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   {{%[0-9]+}} = class_method [[BORROWED_ARG]] : $LazyContainerClass, #LazyContainerClass.lazyVar!getter.1 : (LazyContainerClass) -> () -> Int, $@convention(method) (@guaranteed LazyContainerClass) -> Int
  markUsed(container.lazyVar)
}

// CHECK-LABEL: sil hidden @_T010multi_file25finalVarsAreDevirtualizedyAA18FinalPropertyClassCF
func finalVarsAreDevirtualized(_ obj: FinalPropertyClass) {
  // CHECK: bb0([[ARG:%.*]] : $FinalPropertyClass):
  // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK:   ref_element_addr [[BORROWED_ARG]] : $FinalPropertyClass, #FinalPropertyClass.foo
  // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
  markUsed(obj.foo)
  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: class_method [[BORROWED_ARG]] : $FinalPropertyClass, #FinalPropertyClass.bar!getter.1
  markUsed(obj.bar)
}

// rdar://18448869
// CHECK-LABEL: sil hidden @_T010multi_file34finalVarsDontNeedMaterializeForSetyAA27ObservingPropertyFinalClassCF
func finalVarsDontNeedMaterializeForSet(_ obj: ObservingPropertyFinalClass) {
  obj.foo += 1
  // CHECK: function_ref @_T010multi_file27ObservingPropertyFinalClassC3fooSifg
  // CHECK: function_ref @_T010multi_file27ObservingPropertyFinalClassC3fooSifs
}

// rdar://18503960
// Ensure that we type-check the materializeForSet accessor from the protocol.
class HasComputedProperty: ProtocolWithProperty {
  var foo: Int {
    get { return 1 }
    set {}
  }
}
// CHECK-LABEL: sil hidden [transparent] @_T010multi_file19HasComputedPropertyC3fooSifm : $@convention(method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @guaranteed HasComputedProperty) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
// CHECK-LABEL: sil hidden [transparent] [thunk] @_T010multi_file19HasComputedPropertyCAA012ProtocolWithE0A2aDP3fooSifmTW : $@convention(witness_method) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout HasComputedProperty) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
