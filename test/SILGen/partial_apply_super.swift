
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_class.swiftmodule -module-name resilient_class %S/../Inputs/resilient_class.swift
// RUN: %target-swift-emit-silgen -enable-sil-ownership -module-name partial_apply_super -enable-resilience -parse-as-library -I %t %s | %FileCheck %s

import resilient_class

func doFoo(_ f: () -> ()) {
  f()
}

public class Parent {
  public init() {}
  public func method() {}
  public final func finalMethod() {}
  public class func classMethod() {}
  public final class func finalClassMethod() {}
}

public class GenericParent<A> {
  let a: A
  public init(a: A) {
    self.a = a
  }
  public func method() {}
  public final func finalMethod() {}
  public class func classMethod() {}
  public final class func finalClassMethod() {}
}

class Child : Parent {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super5ChildC6methodyyF : $@convention(method) (@guaranteed Child) -> ()
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $Child):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $Child to $Parent
  // CHECK:   [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @$S19partial_apply_super6ParentC6methodyyFTcTd : $@convention(thin) (@guaranteed Parent) -> @owned @callee_guaranteed () -> ()
  // CHECK:   [[PARTIAL_APPLY:%.*]] = apply [[SUPER_METHOD]]([[BORROWED_CASTED_SELF_COPY]]) : $@convention(thin) (@guaranteed Parent) -> @owned @callee_guaranteed () -> ()
  // CHECK:   [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:   [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super5ChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super5ChildC11classMethodyyFZ : $@convention(method) (@thick Child.Type) -> () {
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type
  // CHECK: [[SUPER_METHOD:%.*]] = function_ref @$S19partial_apply_super6ParentC11classMethodyyFZTcTd : $@convention(thin) (@thick Parent.Type) -> @owned @callee_guaranteed () -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> @owned @callee_guaranteed () -> ()
  // CHECK:   [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super5ChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super5ChildC20callFinalSuperMethodyyF : $@convention(method) (@guaranteed Child) -> ()
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $Child):
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $Child to $Parent
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[SUPER_METHOD:%.*]] = function_ref @$S19partial_apply_super6ParentC11finalMethodyyFTc : $@convention(thin) (@guaranteed Parent) -> @owned @callee_guaranteed () -> ()
  // CHECK:     [[APPLIED_SELF:%.*]] = apply [[SUPER_METHOD]]([[BORROWED_CASTED_SELF_COPY]]) : $@convention(thin) (@guaranteed Parent) -> @owned @callee_guaranteed () -> ()
  // CHECK:     [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[APPLIED_SELF]]
  // CHECK:     [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:     apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super5ChildC20callFinalSuperMethodyyF'
  func callFinalSuperMethod() {
    doFoo(super.finalMethod)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZ : $@convention(method) (@thick Child.Type) -> ()
  // CHECK: bb0([[ARG:%.*]] : @trivial $@thick Child.Type):
  // CHECK:   [[CASTED_SELF:%.*]] = upcast [[ARG]] : $@thick Child.Type to $@thick Parent.Type
  // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @$S19partial_apply_super6ParentC16finalClassMethodyyFZTc : $@convention(thin) (@thick Parent.Type) -> @owned @callee_guaranteed () -> ()
  // CHECK:   [[APPLIED_SELF:%.*]] = apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> @owned @callee_guaranteed () -> ()
  // CHECK:   [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[APPLIED_SELF]]
  // CHECK:   [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZ'
  class func callFinalSuperClassMethod() {
    doFoo(super.finalClassMethod)
  }
}

class GenericChild<A> : GenericParent<A> {
  override init(a: A) {
    super.init(a: a)
  }
  // CHECK-LABEL: sil hidden @$S19partial_apply_super12GenericChildC6methodyyF : $@convention(method) <A> (@guaranteed GenericChild<A>) -> ()
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $GenericChild<A>):
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $GenericChild<A> to $GenericParent<A>
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[SUPER_METHOD:%.*]] = function_ref @$S19partial_apply_super13GenericParentC6methodyyFTcTd : $@convention(thin) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> @owned @callee_guaranteed () -> ()
  // CHECK:     [[PARTIAL_APPLY:%.*]] = apply [[SUPER_METHOD]]<A>([[BORROWED_CASTED_SELF_COPY]]) : $@convention(thin) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> @owned @callee_guaranteed () -> ()
  // CHECK:     [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:     [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:     apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super12GenericChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super12GenericChildC11classMethodyyFZ : $@convention(method) <A> (@thick GenericChild<A>.Type) -> ()
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick GenericChild<A>.Type to $@thick GenericParent<A>.Type
  // CHECK: [[SUPER_METHOD:%.*]] = function_ref @$S19partial_apply_super13GenericParentC11classMethodyyFZTcTd : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> @owned @callee_guaranteed () -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> @owned @callee_guaranteed () -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super12GenericChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class ChildToFixedOutsideParent : OutsideParent {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super25ChildToFixedOutsideParentC6methodyyF
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $ChildToFixedOutsideParent):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $ChildToFixedOutsideParent to $OutsideParent
  // CHECK:   [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:   [[CASTED_SELF_COPY:%.*]] = copy_value [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:   [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:   [[DOWNCAST_BORROWED_CASTED_SELF_COPY:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_SELF_COPY]]
  // Semantic SIL TODO: This super_method used to take
  // ChildToFixedOutsideParent. Since super_method IIRC goes through
  // objc_runtime, this shouldn't matter, but we should look into it before we
  // turn on +0.
  // CHECK:   [[SUPER_METHOD:%.*]] = super_method [[DOWNCAST_BORROWED_CASTED_SELF_COPY]] : $OutsideParent, #OutsideParent.method!1 : (OutsideParent) -> () -> (), $@convention(method) (@guaranteed OutsideParent) -> ()
  // CHECK:   [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF_COPY]]) : $@convention(method) (@guaranteed OutsideParent) -> ()
  // CHECK:   [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:   [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super25ChildToFixedOutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZ
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick ChildToFixedOutsideParent.Type to $@thick OutsideParent.Type
  // CHECK: [[SUPER_METHOD:%.*]] = super_method %0 : $@thick ChildToFixedOutsideParent.Type, #OutsideParent.classMethod!1 : (OutsideParent.Type) -> () -> (), $@convention(method) (@thick OutsideParent.Type) -> (){{.*}}
  // CHECK: [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick OutsideParent.Type) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class ChildToResilientOutsideParent : ResilientOutsideParent {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super29ChildToResilientOutsideParentC6methodyyF : $@convention
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $ChildToResilientOutsideParent):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $ChildToResilientOutsideParent to $ResilientOutsideParent
  // CHECK:   [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:   [[CASTED_SELF_COPY:%.*]] = copy_value [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:   [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:   [[DOWNCAST_BORROWED_CASTED_SELF_COPY:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:   [[SUPER_METHOD:%.*]] = super_method [[DOWNCAST_BORROWED_CASTED_SELF_COPY]] : $ResilientOutsideParent, #ResilientOutsideParent.method!1 : (ResilientOutsideParent) -> () -> (), $@convention(method) (@guaranteed ResilientOutsideParent) -> ()
  // CHECK:   end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // CHECK:   [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF_COPY]]) : $@convention(method) (@guaranteed ResilientOutsideParent) -> ()
  // CHECK:   [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:   [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:   apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super29ChildToResilientOutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZ
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick ChildToResilientOutsideParent.Type to $@thick ResilientOutsideParent.Type
  // CHECK: [[SUPER_METHOD:%.*]] = super_method %0 : $@thick ChildToResilientOutsideParent.Type, #ResilientOutsideParent.classMethod!1 : (ResilientOutsideParent.Type) -> () -> (), $@convention(method) (@thick ResilientOutsideParent.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick ResilientOutsideParent.Type) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GrandchildToFixedOutsideChild : OutsideChild {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyF
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $GrandchildToFixedOutsideChild):
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $GrandchildToFixedOutsideChild to $OutsideChild
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = copy_value [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[DOWNCAST_BORROWED_CASTED_SELF_COPY:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[SUPER_METHOD:%.*]] = super_method [[DOWNCAST_BORROWED_CASTED_SELF_COPY]] : $OutsideChild, #OutsideChild.method!1 : (OutsideChild) -> () -> (), $@convention(method) (@guaranteed OutsideChild) -> ()
  // CHECK:     end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // CHECK:     [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF_COPY]]) : $@convention(method) (@guaranteed OutsideChild) -> ()
  // CHECK:     [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:     [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:     apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZ
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick GrandchildToFixedOutsideChild.Type to $@thick OutsideChild.Type
  // CHECK: [[SUPER_METHOD:%.*]] = super_method %0 : $@thick GrandchildToFixedOutsideChild.Type, #OutsideChild.classMethod!1 : (OutsideChild.Type) -> () -> (), $@convention(method) (@thick OutsideChild.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick OutsideChild.Type) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GrandchildToResilientOutsideChild : ResilientOutsideChild {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyF
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $GrandchildToResilientOutsideChild):
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $GrandchildToResilientOutsideChild to $ResilientOutsideChild
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = copy_value [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[DOWNCAST_BORROWED_CASTED_SELF_COPY:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[SUPER_METHOD:%.*]] = super_method [[DOWNCAST_BORROWED_CASTED_SELF_COPY]] : $ResilientOutsideChild, #ResilientOutsideChild.method!1 : (ResilientOutsideChild) -> () -> (), $@convention(method) (@guaranteed ResilientOutsideChild) -> ()
  // CHEC:      end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // CHECK:     [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF_COPY]]) : $@convention(method) (@guaranteed ResilientOutsideChild) -> ()
  // CHECK:     [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:     [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:     apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZ
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick GrandchildToResilientOutsideChild.Type to $@thick ResilientOutsideChild.Type
  // CHECK: [[SUPER_METHOD:%.*]] = super_method %0 : $@thick GrandchildToResilientOutsideChild.Type, #ResilientOutsideChild.classMethod!1 : (ResilientOutsideChild.Type) -> () -> (), $@convention(method) (@thick ResilientOutsideChild.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick ResilientOutsideChild.Type) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GenericChildToFixedGenericOutsideParent<A> : GenericOutsideParent<A> {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyF
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $GenericChildToFixedGenericOutsideParent<A>):
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $GenericChildToFixedGenericOutsideParent<A> to $GenericOutsideParent<A>
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = copy_value [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[DOWNCAST_BORROWED_CASTED_SELF_COPY:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[SUPER_METHOD:%.*]] = super_method [[DOWNCAST_BORROWED_CASTED_SELF_COPY]] : $GenericOutsideParent<A>, #GenericOutsideParent.method!1 : <A> (GenericOutsideParent<A>) -> () -> (), $@convention(method) <τ_0_0> (@guaranteed GenericOutsideParent<τ_0_0>) -> ()
  // CHECK:     end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // CHECK:     [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]<A>([[CASTED_SELF_COPY]]) : $@convention(method) <τ_0_0> (@guaranteed GenericOutsideParent<τ_0_0>) -> ()
  // CHECK:     [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:     [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:     apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZ
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick GenericChildToFixedGenericOutsideParent<A>.Type to $@thick GenericOutsideParent<A>.Type
  // CHECK: [[SUPER_METHOD:%.*]] = super_method %0 : $@thick GenericChildToFixedGenericOutsideParent<A>.Type, #GenericOutsideParent.classMethod!1 : <A> (GenericOutsideParent<A>.Type) -> () -> (), $@convention(method) <τ_0_0> (@thick GenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@thick GenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GenericChildToResilientGenericOutsideParent<A> : ResilientGenericOutsideParent<A> {
  // CHECK-LABEL: sil hidden @$S19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyF
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $GenericChildToResilientGenericOutsideParent<A>):
  // CHECK:     [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = upcast [[SELF_COPY]] : $GenericChildToResilientGenericOutsideParent<A> to $ResilientGenericOutsideParent<A>
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[CASTED_SELF_COPY:%.*]] = copy_value [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[BORROWED_CASTED_SELF_COPY:%.*]] = begin_borrow [[CASTED_SELF_COPY]]
  // CHECK:     [[DOWNCAST_BORROWED_CASTED_SELF_COPY:%.*]] = unchecked_ref_cast [[BORROWED_CASTED_SELF_COPY]]
  // CHECK:     [[SUPER_METHOD:%.*]] = super_method [[DOWNCAST_BORROWED_CASTED_SELF_COPY]] : $ResilientGenericOutsideParent<A>, #ResilientGenericOutsideParent.method!1 : <A> (ResilientGenericOutsideParent<A>) -> () -> (), $@convention(method) <τ_0_0> (@guaranteed ResilientGenericOutsideParent<τ_0_0>) -> ()
  // CHECK:     end_borrow [[BORROWED_CASTED_SELF_COPY]] from [[CASTED_SELF_COPY]]
  // CHECK:     [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]<A>([[CASTED_SELF_COPY]]) : $@convention(method) <τ_0_0> (@guaranteed ResilientGenericOutsideParent<τ_0_0>) -> ()
  // CHECK:     [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK:     [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK:     apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @$S19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZ
  // CHECK: [[CASTED_SELF:%.*]] = upcast %0 : $@thick GenericChildToResilientGenericOutsideParent<A>.Type to $@thick ResilientGenericOutsideParent<A>.Type
  // CHECK: [[SUPER_METHOD:%.*]] = super_method %0 : $@thick GenericChildToResilientGenericOutsideParent<A>.Type, #ResilientGenericOutsideParent.classMethod!1 : <A> (ResilientGenericOutsideParent<A>.Type) -> () -> (), $@convention(method) <τ_0_0> (@thick ResilientGenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%.*]] = partial_apply [callee_guaranteed] [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@thick ResilientGenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[PARTIAL_APPLY]]
  // CHECK: [[DOFOO:%.*]] = function_ref @$S19partial_apply_super5doFooyyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: apply [[DOFOO]]([[CONVERT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: } // end sil function '$S19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}
