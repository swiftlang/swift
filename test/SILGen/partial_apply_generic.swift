
// RUN: %target-swift-emit-silgen -module-name partial_apply_generic -enable-sil-ownership %s | %FileCheck %s

protocol Panda {
  associatedtype Cuddles : Foo
}

protocol Foo {
  static func staticFunc()
  func instanceFunc()

  func makesSelfNonCanonical<T : Panda>(_: T) where T.Cuddles == Self
}

// CHECK-LABEL: sil hidden @$s21partial_apply_generic14getStaticFunc1{{[_0-9a-zA-Z]*}}F
func getStaticFunc1<T: Foo>(t: T.Type) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic3FooP10staticFunc{{[_0-9a-zA-Z]*}}FZ
// CHECK-NEXT: apply [[REF]]<T>(%0)
  return t.staticFunc
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared [thunk] @$s21partial_apply_generic3FooP10staticFunc{{[_0-9a-zA-Z]*}}FZ
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.staticFunc!1
// CHECK-NEXT: partial_apply [callee_guaranteed] [[REF]]<Self>(%0)
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @$s21partial_apply_generic14getStaticFunc2{{[_0-9a-zA-Z]*}}F
func getStaticFunc2<T: Foo>(t: T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic3FooP10staticFunc{{[_0-9a-zA-Z]*}}FZ
// CHECK: apply [[REF]]<T>
  return T.staticFunc
// CHECK-NOT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @$s21partial_apply_generic16getInstanceFunc1{{[_0-9a-zA-Z]*}}F
func getInstanceFunc1<T: Foo>(t: T) -> () -> () {
// CHECK-NOT: alloc_stack $T
// CHECK-NOT: copy_addr %0 to [initialization]
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: apply [[REF]]<T>
  return t.instanceFunc
// CHECK-NOT: dealloc_stack
// CHECK-NOT: destroy_addr %0 : $*T
// CHECK-NEXT: return
}

// CHECK-LABEL: sil shared [thunk] @$s21partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG:%.*]] : @trivial $*Self):
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.instanceFunc!1
// CHECK-NEXT: [[STACK:%.*]] = alloc_stack $Self
// CHECK-NEXT: copy_addr [[ARG]] to [initialization] [[STACK]]
// CHECK-NEXT: partial_apply [callee_guaranteed] [[REF]]<Self>([[STACK]])
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: return

// CHECK-LABEL: sil hidden @$s21partial_apply_generic16getInstanceFunc2{{[_0-9a-zA-Z]*}}F
func getInstanceFunc2<T: Foo>(t: T) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: partial_apply [callee_guaranteed] [[REF]]<T>(
  return T.instanceFunc
// CHECK-NOT: destroy_addr %0 : $*
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @$s21partial_apply_generic16getInstanceFunc3{{[_0-9a-zA-Z]*}}F
func getInstanceFunc3<T: Foo>(t: T.Type) -> (T) -> () -> () {
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic3FooP12instanceFunc{{[_0-9a-zA-Z]*}}F
// CHECK-NEXT: partial_apply [callee_guaranteed] [[REF]]<T>(
  return t.instanceFunc
// CHECK-NEXT: return
}

// CHECK-LABEL: sil hidden @$s21partial_apply_generic23getNonCanonicalSelfFunc1tyq_cxcxm_t7CuddlesQy_RszAA5PandaR_r0_lF : $@convention(thin) <T, U where T == U.Cuddles, U : Panda> (@thick T.Type) -> @owned @callee_guaranteed (@in_guaranteed T) -> @owned @callee_guaranteed (@in_guaranteed U) -> () {
func getNonCanonicalSelfFunc<T : Foo, U : Panda>(t: T.Type) -> (T) -> (U) -> () where U.Cuddles == T {
// CHECK: [[REF:%.*]] = function_ref @$s21partial_apply_generic3FooP21makesSelfNonCanonicalyyqd__7CuddlesQyd__RszAA5PandaRd__lFTc : $@convention(thin) <τ_0_0><τ_1_0 where τ_0_0 == τ_1_0.Cuddles, τ_1_0 : Panda> (@in_guaranteed τ_0_0) -> @owned @callee_guaranteed (@in_guaranteed τ_1_0) -> ()
// CHECK-NEXT: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[REF]]<T, U>()
  return t.makesSelfNonCanonical
// CHECK-NEXT: return [[CLOSURE]]
}

// curry thunk of Foo.makesSelfNonCanonical<A where ...> (A1) -> ()
// CHECK-LABEL: sil shared [thunk] @$s21partial_apply_generic3FooP21makesSelfNonCanonicalyyqd__7CuddlesQyd__RszAA5PandaRd__lFTc : $@convention(thin) <Self><T where Self == T.Cuddles, T : Panda> (@in_guaranteed Self) -> @owned @callee_guaranteed (@in_guaranteed T) -> () {
// CHECK: bb0([[ARG:%.*]] : @trivial $*Self):
// CHECK: [[REF:%.*]] = witness_method $Self, #Foo.makesSelfNonCanonical!1 : <Self><T where Self == T.Cuddles, T : Panda> (Self) -> (T) -> () : $@convention(witness_method: Foo) <τ_0_0><τ_1_0 where τ_0_0 == τ_1_0.Cuddles, τ_1_0 : Panda> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
// CHECK-NEXT: [[STACK:%.*]] = alloc_stack $Self
// CHECK-NEXT: copy_addr [[ARG]] to [initialization] [[STACK]] : $*Self
// CHECK-NEXT: [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[REF]]<Self, T>([[STACK]])
// CHECK-NEXT: dealloc_stack
// CHECK-NEXT: return [[CLOSURE]]
