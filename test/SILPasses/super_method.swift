// RUN: %target-swift-frontend -emit-sil  %s -use-native-super-method | FileCheck %s

class Parent {
  @inline(never)
  func onlyInParent() {}
  @inline(never)
  final func finalOnlyInParent() {}
  @inline(never)
  func foo() {}
}

class Child : Parent {}

class Grandchild : Child {
  // CHECK: sil hidden @_TFC12super_method10Grandchild16onlyInGrandchildfT_T_
  func onlyInGrandchild() {
    // CHECK-NOT: super_method %0 : $Grandchild, #Parent.onlyInParent!1 : Parent -> () -> ()
    // CHECK: function_ref @_TFC12super_method6Parent12onlyInParentfT_T_
    super.onlyInParent()
    // CHECK: function_ref @_TFC12super_method6Parent17finalOnlyInParentfT_T_
    super.finalOnlyInParent()
  }

  // CHECK: sil hidden @_TFC12super_method10Grandchild3foofT_T_
  override func foo() {
    // CHECK-NOT: super_method %0 : $Grandchild, #Parent.foo!1 : Parent -> () -> ()
    // CHECK: function_ref @_TFC12super_method6Parent3foofT_T_
    super.foo()
  }
}
