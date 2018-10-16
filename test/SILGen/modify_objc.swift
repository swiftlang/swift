// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/modify_objc.h %s | %FileCheck %s
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/modify_objc.h %s -enable-resilience | %FileCheck %s --check-prefix=RESILIENT
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/modify_objc.h %s -enable-testing | %FileCheck %s --check-prefix=TESTING

// REQUIRES: objc_interop

public protocol ProtocolWithBlockProperty {
  // No abstraction difference between the native witness and the requirement.
  var block: ((String?) -> Void)? { get set }

  // Abstraction difference between the native witness and the requirement.
  associatedtype DependentInput
  var dependentBlock: ((DependentInput) -> Void)? { get set }
}

extension ClassWithBlockProperty : ProtocolWithBlockProperty {}

//   Protocol witness for 'block'.
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$sSo22ClassWithBlockPropertyC11modify_objc08ProtocolbcD0A2cDP5blockySSSgcSgvMTW :
// CHECK-SAME:    $@yield_once @convention(witness_method: ProtocolWithBlockProperty) (@inout ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>
// CHECK:    bb0([[SELF_INDIRECT:%.*]] : @trivial $*ClassWithBlockProperty):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow [[SELF_INDIRECT]] : $*ClassWithBlockProperty
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$sSo22ClassWithBlockPropertyC5blockySSSgcSgvM
// CHECK-NEXT: ([[ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: yield [[ADDR]] : $*Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>

// CHECK-LABEL: sil shared [serializable] @$sSo22ClassWithBlockPropertyC5blockySSSgcSgvM :
// CHECK-SAME:    $@yield_once @convention(method) (@guaranteed ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>

//   Protocol witness for 'dependentBlock'
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$sSo22ClassWithBlockPropertyC11modify_objc08ProtocolbcD0A2cDP09dependentC0y14DependentInputQzcSgvMTW :
// CHECK-SAME:    $@yield_once @convention(witness_method: ProtocolWithBlockProperty) (@inout ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>
// CHECK:    bb0([[SELF_INDIRECT:%.*]] : @trivial $*ClassWithBlockProperty):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow [[SELF_INDIRECT]] : $*ClassWithBlockProperty
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$sSo22ClassWithBlockPropertyC09dependentC0ySSSgcSgvM
// CHECK-NEXT: ([[YIELD_ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>
// CHECK-NEXT: [[IN_FUNCTION:%.*]] = load [take] [[YIELD_ADDR]]
// CHECK:    bb3([[OUT_FUNCTION:%.*]] : @owned $Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>):
// CHECK-NEXT: store [[OUT_FUNCTION]] to [init] [[TEMP]] :
// CHECK-NEXT: yield [[TEMP]] : $*Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>

// CHECK-LABEL: sil shared [serializable] @$sSo22ClassWithBlockPropertyC09dependentC0ySSSgcSgvM :
// CHECK-SAME:    $@yield_once @convention(method) (@guaranteed ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>

// Make sure 'modify' implementations for 'dynamic' properties go through
// accessors.
public protocol ProtocolWithIntProperty {
  var x: Int { get set }
}


class HasDynamicStoredProperty : ProtocolWithIntProperty {
  @objc dynamic var x: Int = 0
}

// CHECK-LABEL: sil shared @$s11modify_objc24HasDynamicStoredPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed HasDynamicStoredProperty) -> @yields @inout Int
// CHECK: objc_method %0 : $HasDynamicStoredProperty, #HasDynamicStoredProperty.x!getter.1.foreign
// CHECK: yield
// CHECK: objc_method %0 : $HasDynamicStoredProperty, #HasDynamicStoredProperty.x!setter.1.foreign
// CHECK: return
// CHECK: objc_method %0 : $HasDynamicStoredProperty, #HasDynamicStoredProperty.x!setter.1.foreign
// CHECK: unwind

// TESTING-LABEL: sil shared [serialized] @$s11modify_objc24HasDynamicStoredPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed HasDynamicStoredProperty) -> @yields @inout Int

class HasDynamicComputedProperty : ProtocolWithIntProperty {
  @objc dynamic var x: Int { get { } set { } }
}

// CHECK-LABEL: sil shared @$s11modify_objc26HasDynamicComputedPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed HasDynamicComputedProperty) -> @yields @inout Int
// CHECK: objc_method %0 : $HasDynamicComputedProperty, #HasDynamicComputedProperty.x!getter.1.foreign
// CHECK: yield
// CHECK: objc_method %0 : $HasDynamicComputedProperty, #HasDynamicComputedProperty.x!setter.1.foreign
// CHECK: return
// CHECK: objc_method %0 : $HasDynamicComputedProperty, #HasDynamicComputedProperty.x!setter.1.foreign
// CHECK: unwind

// TESTING-LABEL: sil shared [serialized] @$s11modify_objc26HasDynamicComputedPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed HasDynamicComputedProperty) -> @yields @inout Int

// Make sure 'modify' implementations for public 'dynamic' properties
// are serialized.
public class PublicHasDynamicStoredProperty : ProtocolWithIntProperty {
  @objc public dynamic var x: Int = 0
}

// CHECK-LABEL: sil shared [serialized] @$s11modify_objc30PublicHasDynamicStoredPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed PublicHasDynamicStoredProperty) -> @yields @inout Int
// RESILIENT-LABEL: sil shared [serialized] @$s11modify_objc30PublicHasDynamicStoredPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed PublicHasDynamicStoredProperty) -> @yields @inout Int


public class PublicHasDynamicComputedProperty : ProtocolWithIntProperty {
  @objc public dynamic var x: Int { get { } set { } }
}

// CHECK-LABEL: sil shared [serialized] @$s11modify_objc32PublicHasDynamicComputedPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed PublicHasDynamicComputedProperty) -> @yields @inout Int
// RESILIENT-LABEL: sil shared [serialized] @$s11modify_objc32PublicHasDynamicComputedPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed PublicHasDynamicComputedProperty) -> @yields @inout Int


// ... even if the class inherits NSObject.
public class NSPublicHasDynamicStoredProperty : NSObject, ProtocolWithIntProperty {
  @objc public dynamic var x: Int = 0
}

// CHECK-LABEL: sil shared [serialized] @$s11modify_objc32NSPublicHasDynamicStoredPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed NSPublicHasDynamicStoredProperty) -> @yields @inout Int
// RESILIENT-LABEL: sil shared [serialized] @$s11modify_objc32NSPublicHasDynamicStoredPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed NSPublicHasDynamicStoredProperty) -> @yields @inout Int

public class NSPublicHasDynamicComputedProperty : NSObject, ProtocolWithIntProperty {
  @objc public dynamic var x: Int { get { } set { } }
}

// CHECK-LABEL: sil shared [serialized] @$s11modify_objc34NSPublicHasDynamicComputedPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed NSPublicHasDynamicComputedProperty) -> @yields @inout Int
// RESILIENT-LABEL: sil shared [serialized] @$s11modify_objc34NSPublicHasDynamicComputedPropertyC1xSivM : $@yield_once @convention(method) (@guaranteed NSPublicHasDynamicComputedProperty) -> @yields @inout Int
