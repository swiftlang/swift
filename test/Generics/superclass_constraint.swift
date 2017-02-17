// RUN: %target-typecheck-verify-swift

// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: %FileCheck %s < %t.dump

class A {
  func foo() { }
}

class B : A {
  func bar() { }
}

class Other { }

func f1<T : A>(_: T) where T : Other {} // expected-error{{generic parameter 'T' cannot be a subclass of both 'A' and 'Other'}}
func f2<T : A>(_: T) where T : B {}

class GA<T> {}
class GB<T> : GA<T> {}

protocol P {}

func f3<T, U>(_: T, _: U) where U : GA<T> {}
func f4<T, U>(_: T, _: U) where U : GA<T> {}
func f5<T, U : GA<T>>(_: T, _: U) {}
func f6<U : GA<T>, T : P>(_: T, _: U) {}
func f7<U, T>(_: T, _: U) where U : GA<T>, T : P {}

func f8<T : GA<A>>(_: T) where T : GA<B> {} // expected-error{{generic parameter 'T' cannot be a subclass of both 'GA<A>' and 'GA<B>'}}

func f9<T : GA<A>>(_: T) where T : GB<A> {}
func f10<T : GB<A>>(_: T) where T : GA<A> {}

// FIXME: Extra diagnostics because we're re-building the archetype builder.
func f11<T : GA<T>>(_: T) { } // expected-error 2{{superclass constraint 'T' : 'GA<T>' is recursive}}
func f12<T : GA<U>, U : GB<T>>(_: T, _: U) { } // expected-error 2{{superclass constraint 'U' : 'GB<T>' is recursive}} // expected-error 2{{superclass constraint 'T' : 'GA<U>' is recursive}}
func f13<T : U, U : GA<T>>(_: T, _: U) { } // expected-error{{inheritance from non-protocol, non-class type 'U'}}

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
  // CHECK: Generic signature: <Self where Self : P2, Self.T : C>
  // CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : P2, τ_0_0.T : C>
  func concreteTypeWitnessViaSuperclass1(x: Self.T.T) {}
}

// CHECK: superclassConformance1
// CHECK: Requirements:
// CHECK-NEXT: τ_0_0 : C [Explicit @ {{.*}}:46]
// CHECK-NEXT: τ_0_0 : P3 [Explicit @ {{.*}}:46 -> Superclass (C: P3)]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : C>
func superclassConformance1<T>(t: T) where T : C, T : P3 {}

// CHECK: superclassConformance2
// CHECK: Requirements:
// CHECK-NEXT: τ_0_0 : C [Explicit @ {{.*}}:46]
// CHECK-NEXT: τ_0_0 : P3 [Explicit @ {{.*}}:46 -> Superclass (C: P3)]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : C>
func superclassConformance2<T>(t: T) where T : C, T : P3 {}

protocol P4 { }

class C2 : C, P4 { }

// CHECK: superclassConformance3
// CHECK: Requirements:
// CHECK-NEXT: τ_0_0 : C2 [Explicit @ {{.*}}:46]
// CHECK-NEXT: τ_0_0 : P4 [Explicit @ {{.*}}:46 -> Superclass (C2: P4)]
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : C2>
func superclassConformance3<T>(t: T) where T : C, T : P4, T : C2 {}
