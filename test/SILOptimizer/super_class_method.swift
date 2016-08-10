// RUN: %target-swift-frontend -emit-sil  %s | %FileCheck %s

class Parent {
  @inline(never)
  class func onlyInParent() {}
  @inline(never)
  final class func finalOnlyInParent() {}
  @inline(never)
  class func foo() {}
}

class Child : Parent {}

class Grandchild : Child {
  class func onlyInGrandchild() {
    // CHECK-LABEL: sil hidden @_TZFC18super_class_method10Grandchild16onlyInGrandchildfT_T_
    // CHECK-NOT: super_method %0 : $@thick Grandchild.Type, #Parent.onlyInParent!1 : Parent.Type -> () -> () , $@convention(method) (@thick Parent.Type) -> (){{.*}} // user: %5
    // CHECK: function_ref @_TZFC18super_class_method6Parent12onlyInParentfT_T_
    super.onlyInParent()
    // CHECK: function_ref @_TZFC18super_class_method6Parent17finalOnlyInParentfT_T_
    super.finalOnlyInParent()
  }

  override class func foo() {
    // CHECK: sil hidden @_TZFC18super_class_method10Grandchild3foofT_T_ : $@convention(method) (@thick Grandchild.Type) -> () {
    // CHECK-NOT: super_method %0 : $@thick Grandchild.Type, #Parent.foo!1 : Parent.Type -> () -> () , $@convention(method) (@thick Parent.Type) -> ()
    // CHECK: function_ref @_TZFC18super_class_method6Parent3foofT_T_ 
    super.foo()
  }
}
