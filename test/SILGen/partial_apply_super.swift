
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -I %t -emit-module -emit-module-path=%t/resilient_class.swiftmodule %S/../Inputs/resilient_class.swift

// Note: we build fixed_layout_class without -enable-library-evolution, since with
// -enable-library-evolution even @_fixed_layout classes have resilient metadata, and
// we want to test the fragile access pattern here.

// RUN: %target-swift-frontend -emit-module -I %t -o %t %S/../Inputs/fixed_layout_class.swift

// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name partial_apply_super -enable-library-evolution -parse-as-library -I %t %s | %FileCheck %s

import resilient_class
import fixed_layout_class

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
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super5ChildC6methodyyF : $@convention(method) (@guaranteed Child) -> ()
  // CHECK: bb0([[SELF:%.*]] : @guaranteed $Child):
  // CHECK:   function_ref @$s19partial_apply_super5ChildC6methodyyFyycfu_ : $@convention(thin) (@guaranteed Child) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super5ChildC6methodyyFyycfu_ : $@convention(thin) (@guaranteed Child) -> ()
  // CHECK: function_ref @$s19partial_apply_super6ParentC6methodyyF : $@convention(method) (@guaranteed Parent) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super5ChildC11classMethodyyFZ : $@convention(method) (@thick Child.Type) -> () {
  // CHECK: function_ref @$s19partial_apply_super5ChildC11classMethodyyFZyycfu_ : $@convention(thin) (@thick Child.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super5ChildC11classMethodyyFZyycfu_ : $@convention(thin) (@thick Child.Type) -> ()
  // CHECK: function_ref @$s19partial_apply_super6ParentC11classMethodyyFZ : $@convention(method) (@thick Parent.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC11classMethodyyFZyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super5ChildC20callFinalSuperMethodyyF : $@convention(method) (@guaranteed Child) -> ()
  // CHECK: function_ref @$s19partial_apply_super5ChildC20callFinalSuperMethodyyFyycfu_ : $@convention(thin) (@guaranteed Child) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC20callFinalSuperMethodyyF'
  func callFinalSuperMethod() {
    doFoo(super.finalMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super5ChildC20callFinalSuperMethodyyFyycfu_ : $@convention(thin) (@guaranteed Child) -> ()
  // CHECK: function_ref @$s19partial_apply_super6ParentC11finalMethodyyF : $@convention(method) (@guaranteed Parent) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC20callFinalSuperMethodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZ : $@convention(method) (@thick Child.Type) -> ()
  // CHECK: function_ref @$s19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZyycfu_ : $@convention(thin) (@thick Child.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZ'
  class func callFinalSuperClassMethod() {
    doFoo(super.finalClassMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZyycfu_ : $@convention(thin) (@thick Child.Type) -> ()
  // CHECK: function_ref @$s19partial_apply_super6ParentC16finalClassMethodyyFZ : $@convention(method) (@thick Parent.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super5ChildC25callFinalSuperClassMethodyyFZyycfu_'
}

class GenericChild<A> : GenericParent<A> {
  override init(a: A) {
    super.init(a: a)
  }
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super12GenericChildC6methodyyF : $@convention(method) <A> (@guaranteed GenericChild<A>) -> ()
  // CHECK: function_ref @$s19partial_apply_super12GenericChildC6methodyyFyycfu_ : $@convention(thin) <τ_0_0> (@guaranteed GenericChild<τ_0_0>) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super12GenericChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super12GenericChildC6methodyyFyycfu_ : $@convention(thin) <A> (@guaranteed GenericChild<A>) -> ()
  // CHECK: function_ref @$s19partial_apply_super13GenericParentC6methodyyF : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super12GenericChildC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super12GenericChildC11classMethodyyFZ : $@convention(method) <A> (@thick GenericChild<A>.Type) -> ()
  // CHECK: function_ref @$s19partial_apply_super12GenericChildC11classMethodyyFZyycfu_ : $@convention(thin) <τ_0_0> (@thick GenericChild<τ_0_0>.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super12GenericChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super12GenericChildC11classMethodyyFZyycfu_ : $@convention(thin) <A> (@thick GenericChild<A>.Type) -> ()
  // CHECK: function_ref @$s19partial_apply_super13GenericParentC11classMethodyyFZ : $@convention(method) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super12GenericChildC11classMethodyyFZyycfu_'
}

class ChildToFixedOutsideParent : OutsideParent {
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super25ChildToFixedOutsideParentC6methodyyF
  // CHECK: function_ref @$s19partial_apply_super25ChildToFixedOutsideParentC6methodyyFyycfu_ : $@convention(thin) (@guaranteed ChildToFixedOutsideParent) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super25ChildToFixedOutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super25ChildToFixedOutsideParentC6methodyyFyycfu_ : $@convention(thin) (@guaranteed ChildToFixedOutsideParent) -> ()
  // CHECK: super_method {{%.*}} : $ChildToFixedOutsideParent, #OutsideParent.method : (OutsideParent) -> () -> (), $@convention(method) (@guaranteed OutsideParent) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super25ChildToFixedOutsideParentC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZ
  // CHECK: function_ref @$s19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) (@thick ChildToFixedOutsideParent.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) (@thick ChildToFixedOutsideParent.Type) -> ()
  // CHECK: super_method %0 : $@thick ChildToFixedOutsideParent.Type, #OutsideParent.classMethod : (OutsideParent.Type) -> () -> (), $@convention(method) (@thick OutsideParent.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super25ChildToFixedOutsideParentC11classMethodyyFZyycfu_'
}

class ChildToResilientOutsideParent : ResilientOutsideParent {
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super29ChildToResilientOutsideParentC6methodyyF : $@convention
  // CHECK: function_ref @$s19partial_apply_super29ChildToResilientOutsideParentC6methodyyFyycfu_ : $@convention(thin) (@guaranteed ChildToResilientOutsideParent) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29ChildToResilientOutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super29ChildToResilientOutsideParentC6methodyyFyycfu_ : $@convention(thin) (@guaranteed ChildToResilientOutsideParent) -> ()
  // CHECK: super_method {{%.*}} : $ChildToResilientOutsideParent, #ResilientOutsideParent.method : (ResilientOutsideParent) -> () -> (), $@convention(method) (@guaranteed ResilientOutsideParent) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29ChildToResilientOutsideParentC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZ
  // CHECK: function_ref @$s19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) (@thick ChildToResilientOutsideParent.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) (@thick ChildToResilientOutsideParent.Type) -> ()
  // CHECK: super_method %0 : $@thick ChildToResilientOutsideParent.Type, #ResilientOutsideParent.classMethod : (ResilientOutsideParent.Type) -> () -> (), $@convention(method) (@thick ResilientOutsideParent.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29ChildToResilientOutsideParentC11classMethodyyFZyycfu_'
}

class GrandchildToFixedOutsideChild : OutsideChild {
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyF
  // CHECK: function_ref @$s19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyFyycfu_ : $@convention(thin) (@guaranteed GrandchildToFixedOutsideChild) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyFyycfu_ : $@convention(thin) (@guaranteed GrandchildToFixedOutsideChild) -> ()
  // CHECK: super_method {{%.*}} : $GrandchildToFixedOutsideChild, #OutsideChild.method : (OutsideChild) -> () -> (), $@convention(method) (@guaranteed OutsideChild) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29GrandchildToFixedOutsideChildC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZ
  // CHECK: function_ref @$s19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZyycfu_ : $@convention(thin) (@thick GrandchildToFixedOutsideChild.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZyycfu_ : $@convention(thin) (@thick GrandchildToFixedOutsideChild.Type) -> ()
  // CHECK: super_method %0 : $@thick GrandchildToFixedOutsideChild.Type, #OutsideChild.classMethod : (OutsideChild.Type) -> () -> (), $@convention(method) (@thick OutsideChild.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super29GrandchildToFixedOutsideChildC11classMethodyyFZyycfu_
}

class GrandchildToResilientOutsideChild : ResilientOutsideChild {
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyF
  // CHECK: function_ref @$s19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyFyycfu_ : $@convention(thin) (@guaranteed GrandchildToResilientOutsideChild) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyFyycfu_ : $@convention(thin) (@guaranteed GrandchildToResilientOutsideChild) -> ()
  // CHECK: super_method {{%.*}} : $GrandchildToResilientOutsideChild, #ResilientOutsideChild.method : (ResilientOutsideChild) -> () -> (), $@convention(method) (@guaranteed ResilientOutsideChild) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super33GrandchildToResilientOutsideChildC6methodyyFyycfu_'

  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZ
  // CHECK: function_ref @$s19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZyycfu_ : $@convention(thin) (@thick GrandchildToResilientOutsideChild.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZyycfu_ : $@convention(thin) (@thick GrandchildToResilientOutsideChild.Type) -> ()
  // CHECK: super_method %0 : $@thick GrandchildToResilientOutsideChild.Type, #ResilientOutsideChild.classMethod : (ResilientOutsideChild.Type) -> () -> (), $@convention(method) (@thick ResilientOutsideChild.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super33GrandchildToResilientOutsideChildC11classMethodyyFZyycfu_'
}

class GenericChildToFixedGenericOutsideParent<A> : GenericOutsideParent<A> {
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyF
  // CHECK: function_ref @$s19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyFyycfu_ : $@convention(thin) <τ_0_0> (@guaranteed GenericChildToFixedGenericOutsideParent<τ_0_0>) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyFyycfu_ : $@convention(thin) <A> (@guaranteed GenericChildToFixedGenericOutsideParent<A>) -> ()
  // CHECK: super_method {{%.*}} : $GenericChildToFixedGenericOutsideParent<A>, #GenericOutsideParent.method : <A> (GenericOutsideParent<A>) -> () -> (), $@convention(method) <τ_0_0> (@guaranteed GenericOutsideParent<τ_0_0>) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super019GenericChildToFixedD13OutsideParentC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZ
  // CHECK: function_ref @$s19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) <τ_0_0> (@thick GenericChildToFixedGenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) <A> (@thick GenericChildToFixedGenericOutsideParent<A>.Type) -> ()
  // CHECK: super_method %0 : $@thick GenericChildToFixedGenericOutsideParent<A>.Type, #GenericOutsideParent.classMethod : <A> (GenericOutsideParent<A>.Type) -> () -> (), $@convention(method) <τ_0_0> (@thick GenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super019GenericChildToFixedD13OutsideParentC11classMethodyyFZyycfu_'
}

class GenericChildToResilientGenericOutsideParent<A> : ResilientGenericOutsideParent<A> {
  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyF
  // CHECK: function_ref @$s19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyFyycfu_ : $@convention(thin) <τ_0_0> (@guaranteed GenericChildToResilientGenericOutsideParent<τ_0_0>) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyF'
  override func method() {
    doFoo(super.method)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyFyycfu_ : $@convention(thin) <A> (@guaranteed GenericChildToResilientGenericOutsideParent<A>) -> ()
  // CHECK: super_method %5 : $GenericChildToResilientGenericOutsideParent<A>, #ResilientGenericOutsideParent.method : <A> (ResilientGenericOutsideParent<A>) -> () -> (), $@convention(method) <τ_0_0> (@guaranteed ResilientGenericOutsideParent<τ_0_0>) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super023GenericChildToResilientD13OutsideParentC6methodyyFyycfu_'


  // CHECK-LABEL: sil hidden [ossa] @$s19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZ
  // CHECK: function_ref @$s19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) <τ_0_0> (@thick GenericChildToResilientGenericOutsideParent<τ_0_0>.Type) -> ()
  // CHECK: } // end sil function '$s19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZ'
  override class func classMethod() {
    doFoo(super.classMethod)
  }

  // CHECK-LABEL: sil private [ossa] @$s19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZyycfu_ : $@convention(thin) <A> (@thick GenericChildToResilientGenericOutsideParent<A>.Type) -> ()
  // CHECK: super_method %0 : $@thick GenericChildToResilientGenericOutsideParent<A>.Type, #ResilientGenericOutsideParent.classMethod : <A> (ResilientGenericOutsideParent<A>.Type) -> () -> (), $@convention(method) <τ_0_0> (@thick ResilientGenericOutsideParent<τ_0_0>.Type) -> (
  // CHECK: } // end sil function '$s19partial_apply_super023GenericChildToResilientD13OutsideParentC11classMethodyyFZyycfu_'
}
