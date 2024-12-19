
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -Xllvm -sil-print-types %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class Child : NSObject {}

@objc protocol Parent {
  var children: [Child] { get set }
}

func setChildren(p: Parent, c: Child) {
  p.children = [c]
}

// CHECK-LABEL: sil hidden [ossa] @$s19objc_bridging_array11setChildren1p1cyAA6Parent_p_AA5ChildCtF : $@convention(thin) (@guaranteed any Parent, @guaranteed Child) -> () {
// CHECK: [[OPENED:%.*]] = open_existential_ref %0 : $any Parent to $[[OPENED_TYPE:@opened\(.*, any Parent\) Self]]
// CHECK: [[COPIED:%.*]] = copy_value [[OPENED]] : $[[OPENED_TYPE]]
// CHECK: [[LENGTH:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[FN:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: [[ARRAY_AND_BUFFER:%.*]] = apply [[FN]]<Child>([[LENGTH]]) : $@convention(thin) <τ_0_0> (Builtin.Word) -> (@owned Array<τ_0_0>, Builtin.RawPointer)
// CHECK: ([[ARRAY:%.*]], [[BUFFER_PTR:%.*]]) = destructure_tuple [[ARRAY_AND_BUFFER]] : $(Array<Child>, Builtin.RawPointer)
// CHECK: [[MDI:%.*]] = mark_dependence [[BUFFER_PTR]] : $Builtin.RawPointer on [[ARRAY]]
// CHECK: [[BUFFER:%.*]] = pointer_to_address [[MDI]] : $Builtin.RawPointer to [strict] $*Child
// CHECK: [[CHILD:%.*]] = copy_value %1 : $Child
// CHECK: store [[CHILD]] to [init] [[BUFFER]] : $*Child
// CHECK: [[FIN_FN:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK: [[FIN_ARR:%.*]] = apply [[FIN_FN]]<Child>([[ARRAY]])
// CHECK: [[FN:%.*]] = function_ref @$sSa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @owned NSArray
// CHECK: [[BORROW_ARRAY:%.*]] = begin_borrow [[FIN_ARR]] : $Array<Child>
// CHECK: [[BRIDGED_ARRAY:%.*]] = apply [[FN]]<Child>([[BORROW_ARRAY]]) : $@convention(method) <τ_0_0> (@guaranteed Array<τ_0_0>) -> @owned NSArray
// CHECK: end_borrow [[BORROW_ARRAY]] : $Array<Child>
// CHECK: destroy_value [[FIN_ARR]] : $Array<Child>
// CHECK: [[FN:%.*]] = objc_method [[COPIED]] : $[[OPENED_TYPE]], #Parent.children!setter.foreign : <Self where Self : Parent> (Self) -> ([Child]) -> (), $@convention(objc_method) <τ_0_0 where τ_0_0 : Parent> (NSArray, τ_0_0) -> ()
// CHECK: apply [[FN]]<[[OPENED_TYPE]]>([[BRIDGED_ARRAY]], [[COPIED]]) : $@convention(objc_method) <τ_0_0 where τ_0_0 : Parent> (NSArray, τ_0_0) -> ()
// CHECK: destroy_value [[BRIDGED_ARRAY]] : $NSArray
// CHECK: destroy_value [[COPIED]] : $[[OPENED_TYPE]]
// CHECK: [[RESULT:%.*]] = tuple ()
// CHECK: return [[RESULT]] : $()
