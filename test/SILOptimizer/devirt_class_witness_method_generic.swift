// RUN: %target-swift-frontend -emit-sil -O %s | %FileCheck %s

public protocol P {}

public protocol Q {
  func foo<T: P>(t: T)
  func bar<T: P>(t: T)
}
extension Q {
  @inline(__always)
  public func foo<T: P>(t: T) {
    bar(t: t)
  }

  @_optimize(none)
  public func bar<T: P>(t: T) {}
}

public class C<T>: Q {}

// CHECK-LABEL: sil shared [transparent] [thunk] @$s35devirt_class_witness_method_generic1CCyqd__GAA1QA2aEP3bar1tyqd___tAA1PRd__lFTW : $@convention(witness_method: Q) <τ_0_0><τ_1_0 where τ_0_0 : C<τ_1_0>><τ_2_0 where τ_2_0 : P> (@in_guaranteed τ_2_0, @in_guaranteed τ_0_0) -> () {
// CHECK: [[FN:%.*]] = function_ref @$s35devirt_class_witness_method_generic1QPAAE3bar1tyqd___tAA1PRd__lF : $@convention(method) <τ_0_0 where τ_0_0 : Q><τ_1_0 where τ_1_0 : P> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<τ_0_0, τ_2_0>(%0, %1) : $@convention(method) <τ_0_0 where τ_0_0 : Q><τ_1_0 where τ_1_0 : P> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
