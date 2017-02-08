// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-sil  %s | %FileCheck %s

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
  // CHECK: sil hidden @_T012super_method10GrandchildC06onlyInC0yyF
  func onlyInGrandchild() {
    // CHECK-NOT: super_method %0 : $Grandchild, #Parent.onlyInParent!1 : Parent -> () -> ()
    // CHECK: function_ref @_T012super_method6ParentC06onlyInC0yyF
    super.onlyInParent()
    // CHECK: function_ref @_T012super_method6ParentC011finalOnlyInC0yyF
    super.finalOnlyInParent()
  }

  // CHECK: sil hidden @_T012super_method10GrandchildC3fooyyF
  override func foo() {
    // CHECK-NOT: super_method %0 : $Grandchild, #Parent.foo!1 : Parent -> () -> ()
    // CHECK: function_ref @_T012super_method6ParentC3fooyyF
    super.foo()
  }
}

class GenericParent<A> {
  let a: A
  init(a: A) {
    self.a = a
  }

  func onlyInParent() {}

  @inline(never)
  final func finalOnlyInParent() {}

  @inline(never)
  func method() {}

  @inline(never)
  class func classMethod() {}
}

class GenericChild<A> : GenericParent<A> {}

class GenericGrandchild<A> : GenericChild<A> {
  // CHECK-LABEL: sil hidden @_T012super_method17GenericGrandchildC06onlyInD0yyF : $@convention(method) <A> (@guaranteed GenericGrandchild<A>) -> ()
  func onlyInGrandchild() {
	// CHECK-NOT: super_method %
	// CHECK: function_ref @_T012super_method13GenericParentC06onlyInD0yyF
	// CHECK-NOT: super_method %
    super.onlyInParent()
	// CHECK-NOT: super_method %
    // CHECK: function_ref @_T012super_method13GenericParentC011finalOnlyInD0yyF : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
	// CHECK-NOT: super_method %
    super.finalOnlyInParent()
  }
  // CHECK-LABEL: sil hidden @_T012super_method17GenericGrandchildC0B0yyF : $@convention(method) <A> (@guaranteed GenericGrandchild<A>) -> ()
  override func method() {
	// CHECK-NOT: super_method %
    // CHECK: function_ref @_T012super_method13GenericParentC0B0yyF
	// CHECK-NOT: super_method %
    super.method()
  }
}

class ConcreteChild : GenericParent<String> {
  // CHECK-LABEL: sil hidden @_T012super_method13ConcreteChildCACSS1a_tcfc : $@convention(method) (@owned String, @owned ConcreteChild) -> @owned ConcreteChild
  override init(a: String) {
    // CHECK-NOT: super_method {{%[0-9]+}} : $ConcreteChild, #GenericParent.init!initializer.1
    // CHECK: [[INIT_FN_REF:%[0-9]+]] = function_ref @_T012super_method13GenericParentCACyxGx1a_tcfc : $@convention(method) <τ_0_0> (@in τ_0_0, @owned GenericParent<τ_0_0>) -> @owned GenericParent<τ_0_0>{{.*}} // user: %10
    // CHECK: apply [[INIT_FN_REF]]
    super.init(a: a)
  }
}

class ConcreteGrandchild : ConcreteChild {
  // CHECK-LABEL: sil hidden @_T012super_method18ConcreteGrandchildC06onlyInD0yyF : $@convention(method) (@guaranteed ConcreteGrandchild) -> ()
  func onlyInGrandchild() {
    // CHECK-NOT: super_method {{%[0-9]+}} : $ConcreteGrandchild, #GenericParent.onlyInParent!1
    // CHECK: function_ref @_T012super_method13GenericParentC06onlyInD0yyF : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
    super.onlyInParent()
    // CHECK: function_ref @_T012super_method13GenericParentC011finalOnlyInD0yyF : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
    super.finalOnlyInParent()
  }
  // CHECK-LABEL: sil hidden @_T012super_method18ConcreteGrandchildC0B0yyF : $@convention(method) (@guaranteed ConcreteGrandchild) -> ()
  override func method() {
    // CHECK-NOT: super_method {{%[0-9]+}} : $ConcreteGrandchild, #GenericParent.method!1
    // CHECK: function_ref @_T012super_method13GenericParentC0B0yyF : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
    super.method()
  }
}
