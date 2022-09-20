// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class Base<T: P> {
  init(x: Int = 0) {}
}

class Derived<T: C> : Base<T> {}

protocol P {}
class C: P {}

_ = Derived<C>()

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {
// CHECK: [[FN:%.*]] = function_ref @$s35default_arguments_inherited_generic4BaseC1xACyxGSi_tcfcfA_ : $@convention(thin) <τ_0_0 where τ_0_0 : P> () -> Int
// CHECK: apply [[FN]]<C>() : $@convention(thin) <τ_0_0 where τ_0_0 : P> () -> Int
