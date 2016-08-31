// RUN: %target-swift-frontend -emit-sil  %s | %FileCheck %s

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
  // CHECK-LABEL: sil hidden @_TFC12super_method17GenericGrandchild16onlyInGrandchildfT_T_ : $@convention(method) <A> (@guaranteed GenericGrandchild<A>) -> ()
  func onlyInGrandchild() {
	// CHECK-NOT: super_method %
	// CHECK: function_ref @_TFC12super_method13GenericParent12onlyInParentfT_T_
	// CHECK-NOT: super_method %
    super.onlyInParent()
	// CHECK-NOT: super_method %
    // CHECK: function_ref @_TFC12super_method13GenericParent17finalOnlyInParentfT_T_ : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
	// CHECK-NOT: super_method %
    super.finalOnlyInParent()
  }
  // CHECK-LABEL: sil hidden @_TFC12super_method17GenericGrandchild6methodfT_T_ : $@convention(method) <A> (@guaranteed GenericGrandchild<A>) -> ()
  override func method() {
	// CHECK-NOT: super_method %
    // CHECK: function_ref @_TFC12super_method13GenericParent6methodfT_T_
	// CHECK-NOT: super_method %
    super.method()
  }
}

class ConcreteChild : GenericParent<String> {
  // CHECK-LABEL: sil hidden @_TFC12super_method13ConcreteChildcfT1aSS_S0_ : $@convention(method) (@owned String, @owned ConcreteChild) -> @owned ConcreteChild
  override init(a: String) {
    // CHECK-NOT: super_method {{%[0-9]+}} : $ConcreteChild, #GenericParent.init!initializer.1
    // CHECK: [[INIT_FN_REF:%[0-9]+]] = function_ref @_TFC12super_method13GenericParentcfT1ax_GS0_x_ : $@convention(method) <τ_0_0> (@in τ_0_0, @owned GenericParent<τ_0_0>) -> @owned GenericParent<τ_0_0>{{.*}} // user: %10
    // CHECK: apply [[INIT_FN_REF]]
    super.init(a: a)
  }
}

class ConcreteGrandchild : ConcreteChild {
  // CHECK-LABEL: sil hidden @_TFC12super_method18ConcreteGrandchild16onlyInGrandchildfT_T_ : $@convention(method) (@guaranteed ConcreteGrandchild) -> ()
  func onlyInGrandchild() {
    // CHECK-NOT: super_method {{%[0-9]+}} : $ConcreteGrandchild, #GenericParent.onlyInParent!1
    // CHECK: function_ref @_TFC12super_method13GenericParent12onlyInParentfT_T_ : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
    super.onlyInParent()
    // CHECK: function_ref @_TFC12super_method13GenericParent17finalOnlyInParentfT_T_ : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
    super.finalOnlyInParent()
  }
  // CHECK-LABEL: sil hidden @_TFC12super_method18ConcreteGrandchild6methodfT_T_ : $@convention(method) (@guaranteed ConcreteGrandchild) -> ()
  override func method() {
    // CHECK-NOT: super_method {{%[0-9]+}} : $ConcreteGrandchild, #GenericParent.method!1
    // CHECK: function_ref @_TFC12super_method13GenericParent6methodfT_T_ : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
    super.method()
  }
}
