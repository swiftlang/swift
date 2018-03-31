
// RUN: %target-swift-frontend -module-name specialize_dynamic_self -Xllvm -sil-inline-generics -emit-sil -O -primary-file %s | %FileCheck %s

protocol P {}

extension P {
  @_optimize(none) func method1() {}

  @inline(__always) func method2() { method1() }
}

class C<T> : P {
  // CHECK-LABEL: sil shared [always_inline] @$S23specialize_dynamic_self1CC11returnsSelfACyxGXDyFSi_Tg5 : $@convention(method) (@guaranteed C<Int>) -> @owned C<Int>
  // CHECK: [[RESULT:%.*]] = alloc_stack $C<Int>
  // CHECK: [[FN:%.*]] = function_ref @$S23specialize_dynamic_self1PPAAE7method1yyF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
  // CHECK: apply [[FN]]<@dynamic_self C<Int>>([[RESULT]]) : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()
  // CHECK: return %0 : $C<Int>
  @inline(__always)
  final func returnsSelf() -> Self {
    method2()
    return self
  }
}

// CHECK-LABEL: sil hidden @$S23specialize_dynamic_self8usesCInt1cyAA1CCySiG_tF : $@convention(thin) (@guaranteed C<Int>) -> () {
// CHECK: function_ref @$S23specialize_dynamic_self1CC11returnsSelfACyxGXDyFSi_Tg5 : $@convention(method) (@guaranteed C<Int>) -> @owned C<Int>
// CHECK: return
func usesCInt(c: C<Int>) {
  _ = c.returnsSelf()
}
