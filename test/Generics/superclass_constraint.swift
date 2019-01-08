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

func f1<T : A>(_: T) where T : Other {} // expected-error{{generic parameter 'T' cannot be a subclass of both 'Other' and 'A'}}
// expected-note@-1{{superclass constraint 'T' : 'A' written here}}

func f2<T : A>(_: T) where T : B {}
// expected-warning@-1{{redundant superclass constraint 'T' : 'A'}}
// expected-note@-2{{superclass constraint 'T' : 'B' written here}}


class GA<T> {}
class GB<T> : GA<T> {}

protocol P {}

func f3<T, U>(_: T, _: U) where U : GA<T> {}
func f4<T, U>(_: T, _: U) where U : GA<T> {}
func f5<T, U : GA<T>>(_: T, _: U) {}
func f6<U : GA<T>, T : P>(_: T, _: U) {}
func f7<U, T>(_: T, _: U) where U : GA<T>, T : P {}

func f8<T : GA<A>>(_: T) where T : GA<B> {} // expected-error{{generic parameter 'T' cannot be a subclass of both 'GA<B>' and 'GA<A>'}}
// expected-note@-1{{superclass constraint 'T' : 'GA<A>' written here}}

func f9<T : GA<A>>(_: T) where T : GB<A> {}
// expected-warning@-1{{redundant superclass constraint 'T' : 'GA<A>'}}
// expected-note@-2{{superclass constraint 'T' : 'GB<A>' written here}}

func f10<T : GB<A>>(_: T) where T : GA<A> {}
// expected-warning@-1{{redundant superclass constraint 'T' : 'GA<A>'}}
// expected-note@-2{{superclass constraint 'T' : 'GB<A>' written here}}

func f11<T : GA<T>>(_: T) { } // expected-error{{superclass constraint 'T' : 'GA<T>' is recursive}}
func f12<T : GA<U>, U : GB<T>>(_: T, _: U) { } // expected-error{{superclass constraint 'U' : 'GB<T>' is recursive}} // expected-error{{superclass constraint 'T' : 'GA<U>' is recursive}}
func f13<T : U, U : GA<T>>(_: T, _: U) { } // expected-error{{type 'T' constrained to non-protocol, non-class type 'U'}}

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

// CHECK: superclassConformance1
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : C>
func superclassConformance1<T>(t: T)
  where T : C, // expected-note{{conformance constraint 'T': 'P3' implied here}}
        T : P3 {} // expected-warning{{redundant conformance constraint 'T': 'P3'}}



// CHECK: superclassConformance2
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : C>
func superclassConformance2<T>(t: T)
  where T : C, // expected-note{{conformance constraint 'T': 'P3' implied here}}
   T : P3 {} // expected-warning{{redundant conformance constraint 'T': 'P3'}}

protocol P4 { }

class C2 : C, P4 { }

// CHECK: superclassConformance3
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : C2>
func superclassConformance3<T>(t: T) where T : C, T : P4, T : C2 {}
// expected-warning@-1{{redundant superclass constraint 'T' : 'C'}}
// expected-note@-2{{superclass constraint 'T' : 'C2' written here}}
// expected-warning@-3{{redundant conformance constraint 'T': 'P4'}}
// expected-note@-4{{conformance constraint 'T': 'P4' implied here}}

protocol P5: A { }

protocol P6: A, Other { } // expected-error {{protocol 'P6' cannot be a subclass of both 'Other' and 'A'}}
// expected-error@-1{{multiple inheritance from classes 'A' and 'Other'}}
// expected-note@-2 {{superclass constraint 'Self' : 'A' written here}}

func takeA(_: A) { }
func takeP5<T: P5>(_ t: T) {
	takeA(t) // okay
}

protocol P7 {
	associatedtype Assoc: A, Other 
	// FIXME: expected-error@-1{{multiple inheritance from classes 'A' and 'Other'}}
	// expected-note@-2{{superclass constraint 'Self.Assoc' : 'A' written here}}
	// expected-error@-3{{'Self.Assoc' cannot be a subclass of both 'Other' and 'A'}}
}

// CHECK: superclassConformance4
// CHECK: Generic signature: <T, U where T : P3, U : P3, T.T : C, T.T == U.T>
func superclassConformance4<T: P3, U: P3>(_: T, _: U)
  where T.T: C, // expected-note{{superclass constraint 'T.T' : 'C' written here}}
        U.T: C, // expected-warning{{redundant superclass constraint 'U.T' : 'C'}}
        T.T == U.T { }

// Lookup of superclass associated types from inheritance clause

protocol Elementary {
  associatedtype Element

  func get() -> Element
}

class Classical : Elementary {
  func get() -> Int {
    return 0
  }
}

func genericFunc<T : Elementary, U : Classical>(_: T, _: U) where T.Element == U.Element {}

// Lookup within superclass constraints.
protocol P8 {
  associatedtype B
}

class C8 {
  struct A { }
}

func superclassLookup1<T: C8 & P8>(_: T) where T.A == T.B { }

func superclassLookup2<T: P8>(_: T) where T.A == T.B, T: C8 { }

func superclassLookup3<T>(_: T) where T.A == T.B, T: C8, T: P8 { }

// SR-5165
class C9 {}

protocol P9 {}

class C10 : C9, P9 { }

protocol P10 {
  associatedtype A: C9
}

// CHECK: superclass_constraint.(file).testP10
// CHECK: Generic signature: <T where T : P10, T.A : C10>
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : P10, τ_0_0.A : C10>
func testP10<T>(_: T) where T: P10, T.A: C10 { }

// Nested types of generic class-constrained type parameters.
protocol Tail {
  associatedtype E
}

protocol Rump : Tail {
  associatedtype E = Self
}

class Horse<T>: Rump { }

func hasRedundantConformanceConstraint<X : Horse<T>, T>(_: X) where X : Rump {}
// expected-warning@-1 {{redundant conformance constraint 'X': 'Rump'}}
// expected-note@-2 {{conformance constraint 'X': 'Rump' implied here}}

// SR-5862
protocol X {
	associatedtype Y : A
}

// CHECK-DAG: .noRedundancyWarning@
// CHECK: Generic signature: <C where C : X, C.Y == B>
func noRedundancyWarning<C : X>(_ wrapper: C) where C.Y == B {}

// Qualified lookup bug -- <https://bugs.swift.org/browse/SR-2190>

protocol Init {
  init(x: ())
}

class Base {
  required init(y: ()) {}
}

class Derived : Base {}

func g<T : Init & Derived>(_: T.Type) {
  _ = T(x: ())
  _ = T(y: ())
}

// Binding a class-constrained generic parameter to a subclass existential is
// not sound.
struct G<T : Base> {}
// expected-note@-1 2 {{requirement specified as 'T' : 'Base' [with T = Base & P]}}

_ = G<Base & P>() // expected-error {{'G' requires that 'Base & P' inherit from 'Base'}}

func badClassConstrainedType(_: G<Base & P>) {}
// expected-error@-1 {{'G' requires that 'Base & P' inherit from 'Base'}}
