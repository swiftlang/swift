// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_struct.swiftmodule -module-name resilient_struct %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_class.swiftmodule -module-name resilient_class %S/../Inputs/resilient_class.swift
// RUN: %target-swift-frontend -use-native-super-method -emit-silgen -parse-as-library -I %t %s | FileCheck %s

import resilient_class

func doFoo(f: () -> ()) {
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
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Child, #Parent.method!1 : (Parent) -> () -> () , $@convention(method) (@guaranteed Parent) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@guaranteed Parent) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super5Child11classMethodfT_T_ : $@convention(thin) (@thick Child.Type) -> () {
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick Child.Type, #Parent.classMethod!1 : (Parent.Type) -> () -> () , $@convention(thin) (@thick Parent.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> ()
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

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super5Child25callFinalSuperClassMethodfT_T_ : $@convention(thin) (@thick Child.Type) -> ()
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
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $GenericChild<A>, #GenericParent.method!1 : <A> (GenericParent<A>) -> () -> () , $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
  // CHECK: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil hidden @_TZFC19partial_apply_super12GenericChild11classMethodfT_T_ : $@convention(thin) <A> (@thick GenericChild<A>.Type) -> ()
  // CHECK: [[DOFOO:%[0-9]+]] = function_ref @_TF19partial_apply_super5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  // CHECK: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick GenericChild<A>.Type to $@thick GenericParent<A>.Type
  // CHECK: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick GenericChild<A>.Type, #GenericParent.classMethod!1 : <A> (GenericParent<A>.Type) -> () -> () , $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
  // CHECK: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply %4<A>(%3) : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
  // CHECK: apply %2(%5) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
  override class func classMethod() {
    doFoo(super.classMethod)
  }
}
