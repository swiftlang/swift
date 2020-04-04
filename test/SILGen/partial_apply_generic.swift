
// RUN: %target-swift-emit-silgen -module-name partial_apply_generic %s | %FileCheck %s

protocol Panda {
  associatedtype Cuddles : Foo
}

protocol Foo {
  static func staticFunc()
  func instanceFunc()

  func makesSelfNonCanonical<T : Panda>(_: T) where T.Cuddles == Self
}

// CHECK-LABEL: sil hidden [ossa] @$s21partial_apply_generic14getStaticFunc1{{[_0-9a-zA-Z]*}}F
func getStaticFunc1<T: Foo>(t: T.Type) -> () -> () {
// CHECK: function_ref @$s21partial_apply_generic14getStaticFunc11tyycxm_tAA3FooRzlFyycxmcfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : Foo> (@thick τ_0_0.Type) -> @owned @callee_guaranteed () -> ()
  return t.staticFunc
}

// CHECK-LABEL: sil private [ossa] @$s21partial_apply_generic14getStaticFunc11tyycxm_tAA3FooRzlFyycxmcfu_yycfu0_ : $@convention(thin) <T where T : Foo> (@thick T.Type) -> ()
// CHECK: witness_method $T, #Foo.staticFunc :


// CHECK-LABEL: sil hidden [ossa] @$s21partial_apply_generic14getStaticFunc2{{[_0-9a-zA-Z]*}}F
func getStaticFunc2<T: Foo>(t: T) -> () -> () {
// CHECK: function_ref @$s21partial_apply_generic14getStaticFunc21tyycx_tAA3FooRzlFyycxmcfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : Foo> (@thick τ_0_0.Type) -> @owned @callee_guaranteed () -> ()
  return T.staticFunc
}

// CHECK-LABEL: sil private [ossa] @$s21partial_apply_generic14getStaticFunc21tyycx_tAA3FooRzlFyycxmcfu_yycfu0_ : $@convention(thin) <T where T : Foo> (@thick T.Type) -> ()
// CHECK: witness_method $T, #Foo.staticFunc :


// CHECK-LABEL: sil hidden [ossa] @$s21partial_apply_generic16getInstanceFunc1{{[_0-9a-zA-Z]*}}F
func getInstanceFunc1<T: Foo>(t: T) -> () -> () {
// CHECK: function_ref @$s21partial_apply_generic16getInstanceFunc11tyycx_tAA3FooRzlFyycxcfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed () -> ()
  return t.instanceFunc
}

// CHECK-LABEL: sil private [ossa] @$s21partial_apply_generic16getInstanceFunc11tyycx_tAA3FooRzlFyycxcfu_yycfu0_ : $@convention(thin) <T where T : Foo> (@in_guaranteed T) -> () {
// CHECK: witness_method $T, #Foo.instanceFunc : <Self where Self : Foo> (Self) -> () -> () : $@convention(witness_method: Foo) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()


// CHECK-LABEL: sil hidden [ossa] @$s21partial_apply_generic16getInstanceFunc2{{[_0-9a-zA-Z]*}}F
func getInstanceFunc2<T: Foo>(t: T) -> (T) -> () -> () {
// CHECK: function_ref @$s21partial_apply_generic16getInstanceFunc21tyycxcx_tAA3FooRzlFyycxcfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed () -> ()
  return T.instanceFunc
}

// CHECK-LABEL: sil private [ossa] @$s21partial_apply_generic16getInstanceFunc21tyycxcx_tAA3FooRzlFyycxcfu_yycfu0_ : $@convention(thin) <T where T : Foo> (@in_guaranteed T) -> ()
// CHECK: witness_method $T, #Foo.instanceFunc : <Self where Self : Foo> (Self) -> () -> () : $@convention(witness_method: Foo) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()


// CHECK-LABEL: sil hidden [ossa] @$s21partial_apply_generic16getInstanceFunc3{{[_0-9a-zA-Z]*}}F
func getInstanceFunc3<T: Foo>(t: T.Type) -> (T) -> () -> () {
// CHECK:  function_ref @$s21partial_apply_generic16getInstanceFunc31tyycxcxm_tAA3FooRzlFyycxcfu_ : $@convention(thin) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed () -> ()
  return t.instanceFunc
}

// CHECK-LABEL: sil private [ossa] @$s21partial_apply_generic16getInstanceFunc31tyycxcxm_tAA3FooRzlFyycxcfu_yycfu0_ : $@convention(thin) <T where T : Foo> (@in_guaranteed T) -> ()
// CHECK: witness_method $T, #Foo.instanceFunc : <Self where Self : Foo> (Self) -> () -> () : $@convention(witness_method: Foo) <τ_0_0 where τ_0_0 : Foo> (@in_guaranteed τ_0_0) -> ()


// CHECK-LABEL: sil hidden [ossa] @$s21partial_apply_generic23getNonCanonicalSelfFunc1tyq_cxcxm_t7CuddlesQy_RszAA5PandaR_r0_lF : $@convention(thin) <T, U where T == U.Cuddles, U : Panda> (@thick T.Type) -> @owned @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> (@owned @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_1>) for <T, U> {
func getNonCanonicalSelfFunc<T, U : Panda>(t: T.Type) -> (T) -> (U) -> () where U.Cuddles == T {
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic23getNonCanonicalSelfFunc1tyq_cxcxm_t7CuddlesQy_RszAA5PandaR_r0_lFyq_cxcfu_ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 == τ_0_1.Cuddles, τ_0_1 : Panda> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> () for <τ_0_1>
  return t.makesSelfNonCanonical
}

// CHECK-LABEL: sil private [ossa] @$s21partial_apply_generic23getNonCanonicalSelfFunc1tyq_cxcxm_t7CuddlesQy_RszAA5PandaR_r0_lFyq_cxcfu_yq_cfu0_ : $@convention(thin) <T, U where T == U.Cuddles, U : Panda> (@in_guaranteed U, @in_guaranteed T) -> () {
// CHECK: witness_method $T, #Foo.makesSelfNonCanonical : <Self><T where Self == T.Cuddles, T : Panda> (Self) -> (T) -> () : $@convention(witness_method: Foo) <τ_0_0><τ_1_0 where τ_0_0 == τ_1_0.Cuddles, τ_1_0 : Panda> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
