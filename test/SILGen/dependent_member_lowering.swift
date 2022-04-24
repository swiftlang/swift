
// RUN: %target-swift-emit-silgen -module-name dependent_member_lowering %s | %FileCheck %s

protocol P {
  associatedtype A

  func f(_ x: A)
}
struct Foo<T>: P {
  typealias A = T.Type

  func f(_ t: T.Type) {}
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s25dependent_member_lowering3FooVyxGAA1PA2aEP1fyy1AQzFTW : $@convention(witness_method: P) <τ_0_0> (@in_guaranteed @thick τ_0_0.Type, @in_guaranteed Foo<τ_0_0>) -> ()
  // CHECK:       bb0(%0 : $*@thick τ_0_0.Type, %1 : $*Foo<τ_0_0>):
}
struct Bar<T>: P {
  typealias A = (Int) -> T

  func f(_ t: @escaping (Int) -> T) {}
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s25dependent_member_lowering3BarVyxGAA1PA2aEP1fyy1AQzFTW : $@convention(witness_method: P) <τ_0_0> (@in_guaranteed @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Int, τ_0_0>, @in_guaranteed Bar<τ_0_0>) -> ()
  // CHECK:       bb0(%0 : $*@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Int, τ_0_0>, %1 : $*Bar<τ_0_0>):
}

protocol P0 {
  associatedtype Foo
  func foo() -> Foo
}

protocol P1: P0 where Foo == String {
  func foo() -> String
}

// CHECK-LABEL: sil hidden [ossa] @$s25dependent_member_lowering26existentialDependentMemberySSAA2P1_pF : $@convention(thin) (@in_guaranteed P1) -> @owned String
func existentialDependentMember(_ p1: any P1) -> String {
  // CHECK: $@convention(witness_method: P0) <τ_0_0 where τ_0_0 : P0> (@in_guaranteed τ_0_0) -> @out τ_0_0.Foo
  return p1.foo()
}
