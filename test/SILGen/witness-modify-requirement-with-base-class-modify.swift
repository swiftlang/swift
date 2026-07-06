// RUN: %target-swift-emit-silgen %s | %FileCheck %s

public protocol P {
  associatedtype A
}

public class Base<T: P> {
  public var foo: T.A?
}

public struct S {}

public protocol Q {
  var foo: S? {set get}
}

public class Derived<T: P> : Base<T>, Q where T.A == S {}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s4main7DerivedCyxGAA1QA2aEP3fooAA1SVSgvgTW : $@convention(witness_method: Q) <τ_0_0 where τ_0_0 : P, τ_0_0.A == S> (@in_guaranteed Derived<τ_0_0>) -> Optional<S> {
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s4main7DerivedCyxGAA1QA2aEP3fooAA1SVSgvsTW : $@convention(witness_method: Q) <τ_0_0 where τ_0_0 : P, τ_0_0.A == S> (Optional<S>, @inout Derived<τ_0_0>) -> () {
// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s4main7DerivedCyxGAA1QA2aEP3fooAA1SVSgvMTW : $@yield_once @convention(witness_method: Q) <τ_0_0 where τ_0_0 : P, τ_0_0.A == S> @substituted <τ_0_0> (@inout τ_0_0) -> @yields @inout Optional<S> for <Derived<τ_0_0>> {

// CHECK-LABEL: sil_witness_table [serialized] <T where T : P, T.A == S> Derived<T>: Q module main {
// CHECK-NEXT:    method #Q.foo!getter: <Self where Self : Q> (Self) -> () -> S? : @$s4main7DerivedCyxGAA1QA2aEP3fooAA1SVSgvgTW
// CHECK-NEXT:    method #Q.foo!setter: <Self where Self : Q> (inout Self) -> (S?) -> () : @$s4main7DerivedCyxGAA1QA2aEP3fooAA1SVSgvsTW
// CHECK-NEXT:    method #Q.foo!modify: <Self where Self : Q> (inout Self) -> () -> () : @$s4main7DerivedCyxGAA1QA2aEP3fooAA1SVSgvMTW
// CHECK-NEXT:  }