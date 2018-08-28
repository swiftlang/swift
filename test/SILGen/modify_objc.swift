// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/modify_objc.h %s | %FileCheck %s

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
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$SSo22ClassWithBlockPropertyC11modify_objc08ProtocolbcD0A2cDP5blockySSSgcSgvMTW :
// CHECK-SAME:    $@yield_once @convention(witness_method: ProtocolWithBlockProperty) (@inout ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>
// CHECK:    bb0([[SELF_INDIRECT:%.*]] : @trivial $*ClassWithBlockProperty):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow [[SELF_INDIRECT]] : $*ClassWithBlockProperty
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$SSo22ClassWithBlockPropertyC5blockySSSgcSgvM
// CHECK-NEXT: ([[ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: yield [[ADDR]] : $*Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>

// CHECK-LABEL: sil shared [serializable] @$SSo22ClassWithBlockPropertyC5blockySSSgcSgvM :
// CHECK-SAME:    $@yield_once @convention(method) (@guaranteed ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>

//   Protocol witness for 'dependentBlock'
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] @$SSo22ClassWithBlockPropertyC11modify_objc08ProtocolbcD0A2cDP09dependentC0y14DependentInputQzcSgvMTW :
// CHECK-SAME:    $@yield_once @convention(witness_method: ProtocolWithBlockProperty) (@inout ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>
// CHECK:    bb0([[SELF_INDIRECT:%.*]] : @trivial $*ClassWithBlockProperty):
// CHECK-NEXT: [[SELF:%.*]] = load_borrow [[SELF_INDIRECT]] : $*ClassWithBlockProperty
// CHECK-NEXT: // function_ref
// CHECK-NEXT: [[FN:%.*]] = function_ref @$SSo22ClassWithBlockPropertyC09dependentC0ySSSgcSgvM
// CHECK-NEXT: ([[YIELD_ADDR:%.*]], [[TOKEN:%.*]]) = begin_apply [[FN]]([[SELF]])
// CHECK-NEXT: [[TEMP:%.*]] = alloc_stack $Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>
// CHECK-NEXT: [[IN_FUNCTION:%.*]] = load [take] [[YIELD_ADDR]]
// CHECK:    bb3([[OUT_FUNCTION:%.*]] : @owned $Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>):
// CHECK-NEXT: store [[OUT_FUNCTION]] to [init] [[TEMP]] :
// CHECK-NEXT: yield [[TEMP]] : $*Optional<@callee_guaranteed (@in_guaranteed Optional<String>) -> ()>

// CHECK-LABEL: sil shared [serializable] @$SSo22ClassWithBlockPropertyC09dependentC0ySSSgcSgvM :
// CHECK-SAME:    $@yield_once @convention(method) (@guaranteed ClassWithBlockProperty) -> @yields @inout Optional<@callee_guaranteed (@guaranteed Optional<String>) -> ()>
