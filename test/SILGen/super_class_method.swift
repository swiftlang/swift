// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -use-native-super-method | FileCheck %s

class Parent {
  class func onlyInParent() {}
  final class func finalOnlyInParent() {}
  class func foo() {}
}

class Child : Parent {}

class Grandchild : Child {
  class func onlyInGrandchild() {
    // CHECK-LABEL: sil hidden @_TZFC18super_class_method10Grandchild16onlyInGrandchildfT_T_
    // CHECK-NOT: function_ref @_TZFC18super_class_method6Parent12onlyInParentfT_T_
    // CHECK: super_method %0 : $@thick Grandchild.Type, #Parent.onlyInParent!1 : Parent.Type -> () -> () , $@convention(thin) (@thick Parent.Type) -> () // user: %5
    super.onlyInParent()
    // CHECK: function_ref @_TZFC18super_class_method6Parent17finalOnlyInParentfT_T_
    super.finalOnlyInParent()
  }

  override class func foo() {
    // CHECK: sil hidden @_TZFC18super_class_method10Grandchild3foofT_T_ : $@convention(thin) (@thick Grandchild.Type) -> () {
    // CHECK-NOT: function_ref @_TZFC18super_class_method10Grandchild3foofT_T_
    // CHECK: super_method %0 : $@thick Grandchild.Type, #Parent.foo!1 : Parent.Type -> () -> () , $@convention(thin) (@thick Parent.Type) -> ()
    super.foo()
  }
}
