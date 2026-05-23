// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P<A> {
  associatedtype A
}
protocol Q<A> {
  associatedtype A
}

struct G<T>: Q, P {
  typealias A = T
}

struct ConcreteTest {
  // CHECK: Opaque result type of opaque_signature.(file).ConcreteTest.f1()@
  // CHECK-NEXT: Opaque result signature: <τ_0_0 where τ_0_0 : Q>
  func f1() -> some Q { return G<Int>() }

  // CHECK: Opaque result type of opaque_signature.(file).ConcreteTest.f2()@
  // CHECK-NEXT: Opaque result signature: <τ_0_0 where τ_0_0 : Q, τ_0_0.[Q]A == Int>
  func f2() -> some Q<Int> { return G<Int>() }

  // CHECK: Opaque result type of opaque_signature.(file).ConcreteTest.f3()@
  // CHECK-NEXT: Opaque result signature: <τ_0_0 where τ_0_0 : P, τ_0_0 : Q>
  func f3() -> some P & Q { return G<Int>() }
}

struct GenericTest<T: P> {
  // CHECK: Opaque result type of opaque_signature.(file).GenericTest.f1()@
  // CHECK-NEXT: Opaque result signature: <T, τ_1_0 where T : P, τ_1_0 : Q>
  func f1() -> some Q { return G<T.A>() }

  // CHECK: Opaque result type of opaque_signature.(file).GenericTest.f2()@
  // CHECK-NEXT: Opaque result signature: <T, τ_1_0 where T : P, T.[P]A == τ_1_0.[Q]A, τ_1_0 : Q>
  func f2() -> some Q<T.A> { return G<T.A>() }
}
