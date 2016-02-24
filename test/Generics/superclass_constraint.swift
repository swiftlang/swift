// RUN: %target-parse-verify-swift

// RUN: %target-parse-verify-swift -parse -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: FileCheck %s < %t.dump

// rdar://problem/24730536
// Superclass constraints can be used to resolve nested types to concrete types.

protocol P3 {
  associatedtype T
}

protocol P2 {
  associatedtype T : P3
}

class C : P3 {
  typealias T = Int
}

class S : P2 {
  typealias T = C
}

extension P2 where Self.T : C {
  // CHECK: superclass_constraint.(file).P2.concreteTypeWitnessViaSuperclass1
  // CHECK: Generic signature: <Self where Self : P2, Self.T : C, Self.T : P3, Self.T.T == T>
  // CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : P2, τ_0_0.T : C, τ_0_0.T : P3, τ_0_0.T.T == Int>
  func concreteTypeWitnessViaSuperclass1(x: Self.T.T) {}
}
