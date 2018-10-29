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
    // CHECK-LABEL: sil hidden @$s18super_class_method10GrandchildC06onlyInD0yyFZ
    // CHECK-NOT: super_method %0 : $@thick Grandchild.Type, #Parent.onlyInParent!1 : Parent.Type -> () -> (), $@convention(method) (@thick Parent.Type) -> (){{.*}} // user: %5
    // CHECK: function_ref @$s18super_class_method6ParentC06onlyInD0yyFZ
    super.onlyInParent()
    // CHECK: function_ref @$s18super_class_method6ParentC011finalOnlyInD0yyFZ
    super.finalOnlyInParent()
  }

  override class func foo() {
    // CHECK: sil hidden @$s18super_class_method10GrandchildC3fooyyFZ : $@convention(method) (@thick Grandchild.Type) -> () {
    // CHECK-NOT: super_method %0 : $@thick Grandchild.Type, #Parent.foo!1 : Parent.Type -> () -> (), $@convention(method) (@thick Parent.Type) -> ()
    // CHECK: function_ref @$s18super_class_method6ParentC3fooyyFZ 
    super.foo()
  }
}
