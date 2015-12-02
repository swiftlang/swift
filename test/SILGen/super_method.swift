// RUN: %target-swift-frontend -emit-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -use-native-super-method | FileCheck %s

class Parent {
  func onlyInParent() {}
  final func finalOnlyInParent() {}
  func foo() {}
}

class Child : Parent {}

class Grandchild : Child {
  // CHECK: sil hidden @_TFC12super_method10Grandchild16onlyInGrandchildfT_T_
  func onlyInGrandchild() {
    // CHECK: super_method %0 : $Grandchild, #Parent.onlyInParent!1 : Parent -> () -> ()
    super.onlyInParent()
    // CHECK: function_ref super_method.Parent.finalOnlyInParent
    super.finalOnlyInParent()
  }

  // CHECK: sil hidden @_TFC12super_method10Grandchild3foofT_T_
  override func foo() {
    // CHECK: super_method %0 : $Grandchild, #Parent.foo!1 : Parent -> () -> ()
    super.foo()
  }
}
