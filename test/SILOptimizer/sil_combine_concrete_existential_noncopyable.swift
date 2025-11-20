// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

protocol P: ~Copyable {}

@_optimize(none)
func f<T: P>(_: T) {}

public struct S: P {
  @_optimize(none)
  init() {}
}

// CHECK-LABEL: sil @$s44sil_combine_concrete_existential_noncopyable1gyyF : $@convention(thin) () -> () {
// CHECK: [[S_ADDR:%.*]] =  alloc_stack [lexical] [var_decl] $S
// CHECK: [[INIT_FN:%.*]] = function_ref @$s44sil_combine_concrete_existential_noncopyable1SVACycfC : $@convention(method) (@thin S.Type) -> S
// CHECK: [[S:%.*]] = apply [[INIT_FN]]({{%.*}}) : $@convention(method) (@thin S.Type) -> S
// CHECK: store [[S]] to [[S_ADDR]]
// CHECK: [[CALLEE_FN:%.*]] = function_ref @$s44sil_combine_concrete_existential_noncopyable1fyyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[CALLEE_FN]]<S>([[S_ADDR]])
// CHECK: return

public func g() {
  let e: any P = S()
  f(e)
}
