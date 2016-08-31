// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_class.swiftmodule -module-name resilient_class %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -enable-resilience -emit-silgen -parse-as-library -I %t %s | %FileCheck %s

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
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super5Child6methodfT_T_ : $@convention(method) (@guaranteed Child) -> ()
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Child to $Parent
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TTdFC19partial_apply_super6Parent6methodFT_T_ : $@convention(thin) (@owned Parent) -> @owned @callee_owned () -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@owned Parent) -> @owned @callee_owned () -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super5Child11classMethodfT_T_ : $@convention(method) (@thick Child.Type) -> () {
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TTdZFC19partial_apply_super6Parent11classMethodFT_T_ : $@convention(thin) (@thick Parent.Type) -> @owned @callee_owned () -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> @owned @callee_owned () -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super5Child20callFinalSuperMethodfT_T_ : $@convention(method) (@guaranteed Child) -> () 
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $Child to $Parent
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC19partial_apply_super6Parent11finalMethodFT_T_ : $@convention(thin) (@owned Parent) -> @owned @callee_owned () -> ()
  // CHECK: [[APPLIED_SELF:%[0-9]+]] = apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@owned Parent) -> @owned @callee_owned () -> ()
  // CHECK:  apply [[DOFOO]]([[APPLIED_SELF]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  func callFinalSuperMethod() {
    doFoo(super.finalMethod)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super5Child25callFinalSuperClassMethodfT_T_ : $@convention(method) (@thick Child.Type) -> ()
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TZFC19partial_apply_super6Parent16finalClassMethodFT_T_ : $@convention(thin) (@thick Parent.Type) -> @owned @callee_owned () -> ()
  // CHECK: [[APPLIED_SELF:%[0-9]+]] = apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> @owned @callee_owned () -> ()
  // CHECK: apply [[DOFOO]]([[APPLIED_SELF]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  class func callFinalSuperClassMethod() {
    doFoo(super.finalClassMethod)
  }
}

class GenericChild<A> : GenericParent<A> {
  override init(a: A) {
    super.init(a: a)
  }
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super12GenericChild6methodfT_T_ : $@convention(method) <A> (@guaranteed GenericChild<A>) -> ()
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $GenericChild<A> to $GenericParent<A>
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TTdFC19partial_apply_super13GenericParent6methodFT_T_ : $@convention(thin) <τ_0_0> (@owned GenericParent<τ_0_0>) -> @owned @callee_owned () -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(thin) <τ_0_0> (@owned GenericParent<τ_0_0>) -> @owned @callee_owned () -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super12GenericChild11classMethodfT_T_ : $@convention(method) <A> (@thick GenericChild<A>.Type) -> ()
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick GenericChild<A>.Type to $@thick GenericParent<A>.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TTdZFC19partial_apply_super13GenericParent11classMethodFT_T_ : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> @owned @callee_owned () -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = apply %4<A>(%3) : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> @owned @callee_owned () -> ()
  // CHECK: apply %2(%5) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class ChildToFixedOutsideParent : OutsideParent {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super25ChildToFixedOutsideParent6methodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $ChildToFixedOutsideParent to $OutsideParent
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $ChildToFixedOutsideParent, #OutsideParent.method!1 : (OutsideParent) -> () -> () , $@convention(method) (@guaranteed OutsideParent) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@guaranteed OutsideParent) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super25ChildToFixedOutsideParent11classMethodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick ChildToFixedOutsideParent.Type to $@thick OutsideParent.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick ChildToFixedOutsideParent.Type, #OutsideParent.classMethod!1 : (OutsideParent.Type) -> () -> () , $@convention(method) (@thick OutsideParent.Type) -> (){{.*}} // user: %5
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick OutsideParent.Type) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class ChildToResilientOutsideParent : ResilientOutsideParent {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super29ChildToResilientOutsideParent6methodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $ChildToResilientOutsideParent to $ResilientOutsideParent
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $ChildToResilientOutsideParent, #ResilientOutsideParent.method!1 : (ResilientOutsideParent) -> () -> () , $@convention(method) (@guaranteed ResilientOutsideParent) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@guaranteed ResilientOutsideParent) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super29ChildToResilientOutsideParent11classMethodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick ChildToResilientOutsideParent.Type to $@thick ResilientOutsideParent.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick ChildToResilientOutsideParent.Type, #ResilientOutsideParent.classMethod!1 : (ResilientOutsideParent.Type) -> () -> () , $@convention(method) (@thick ResilientOutsideParent.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick ResilientOutsideParent.Type) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GrandchildToFixedOutsideChild : OutsideChild {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super29GrandchildToFixedOutsideChild6methodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $GrandchildToFixedOutsideChild to $OutsideChild
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $GrandchildToFixedOutsideChild, #OutsideChild.method!1 : (OutsideChild) -> () -> () , $@convention(method) (@guaranteed OutsideChild) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply %5(%4) : $@convention(method) (@guaranteed OutsideChild) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super29GrandchildToFixedOutsideChild11classMethodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick GrandchildToFixedOutsideChild.Type to $@thick OutsideChild.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick GrandchildToFixedOutsideChild.Type, #OutsideChild.classMethod!1 : (OutsideChild.Type) -> () -> () , $@convention(method) (@thick OutsideChild.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply %4(%3) : $@convention(method) (@thick OutsideChild.Type) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GrandchildToResilientOutsideChild : ResilientOutsideChild {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super33GrandchildToResilientOutsideChild6methodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $GrandchildToResilientOutsideChild to $ResilientOutsideChild
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $GrandchildToResilientOutsideChild, #ResilientOutsideChild.method!1 : (ResilientOutsideChild) -> () -> () , $@convention(method) (@guaranteed ResilientOutsideChild) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@guaranteed ResilientOutsideChild) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super33GrandchildToResilientOutsideChild11classMethodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick GrandchildToResilientOutsideChild.Type to $@thick ResilientOutsideChild.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick GrandchildToResilientOutsideChild.Type, #ResilientOutsideChild.classMethod!1 : (ResilientOutsideChild.Type) -> () -> () , $@convention(method) (@thick ResilientOutsideChild.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@thick ResilientOutsideChild.Type) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GenericChildToFixedGenericOutsideParent<A> : GenericOutsideParent<A> {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super39GenericChildToFixedGenericOutsideParent6methodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $GenericChildToFixedGenericOutsideParent<A> to $GenericOutsideParent<A>
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $GenericChildToFixedGenericOutsideParent<A>, #GenericOutsideParent.method!1 : <A> (GenericOutsideParent<A>) -> () -> () , $@convention(method) <τ_0_0> (@guaranteed GenericOutsideParent<τ_0_0>) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@guaranteed GenericOutsideParent<τ_0_0>) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super39GenericChildToFixedGenericOutsideParent11classMethodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick GenericChildToFixedGenericOutsideParent<A>.Type to $@thick GenericOutsideParent<A>.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick GenericChildToFixedGenericOutsideParent<A>.Type, #GenericOutsideParent.classMethod!1 : <A> (GenericOutsideParent<A>.Type) -> () -> () , $@convention(method) <τ_0_0> (@thick GenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@thick GenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}

class GenericChildToResilientGenericOutsideParent<A> : ResilientGenericOutsideParent<A> {
  // CHECK-LABEL: sil hidden @_TFC19partial_apply_super43GenericChildToResilientGenericOutsideParent6methodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $GenericChildToResilientGenericOutsideParent<A> to $ResilientGenericOutsideParent<A>
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $GenericChildToResilientGenericOutsideParent<A>, #ResilientGenericOutsideParent.method!1 : <A> (ResilientGenericOutsideParent<A>) -> () -> () , $@convention(method) <τ_0_0> (@guaranteed ResilientGenericOutsideParent<τ_0_0>) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@guaranteed ResilientGenericOutsideParent<τ_0_0>) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super43GenericChildToResilientGenericOutsideParent11classMethodfT_T_
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick GenericChildToResilientGenericOutsideParent<A>.Type to $@thick ResilientGenericOutsideParent<A>.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick GenericChildToResilientGenericOutsideParent<A>.Type, #ResilientGenericOutsideParent.classMethod!1 : <A> (ResilientGenericOutsideParent<A>.Type) -> () -> () , $@convention(method) <τ_0_0> (@thick ResilientGenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@thick ResilientGenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}
