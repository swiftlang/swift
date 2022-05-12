// RUN: %target-swift-emit-silgen %s | %FileCheck %s

public protocol P {
  associatedtype A : Q where A.B == Self

  func hasDefaultImplementation1()
  func hasDefaultImplementation2<U>(_: U) where U : Q, U.B == Self
}

public extension P {
  func hasDefaultImplementation1() {}
  func hasDefaultImplementation2<U>(_: U) where U : Q, U.B == Self {}
}

public protocol Q {
  associatedtype B
}

public class C : P {
  public typealias A = D
}

public class D : Q {
  public typealias B = C
}

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s41class_conforms_with_default_concrete_self1CCAA1PA2aDP25hasDefaultImplementation1yyFTW : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : C> (@in_guaranteed τ_0_0) -> () {

// CHECK-LABEL: sil shared [transparent] [serialized] [thunk] [ossa] @$s41class_conforms_with_default_concrete_self1CCAA1PA2aDP25hasDefaultImplementation2yyqd__1BQyd__RszAA1QRd__lFTW : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : C><τ_1_0 where τ_0_0 == τ_1_0.B, τ_1_0 : Q> (@in_guaranteed τ_1_0, @in_guaranteed τ_0_0) -> () {
