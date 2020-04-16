// RUN: %target-typecheck-verify-swift

// RUN: %target-typecheck-verify-swift -debug-generic-signatures > %t.dump 2>&1
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

func f11<T : U, U : GA<T>>(_: T, _: U) { } // expected-error{{type 'T' constrained to non-protocol, non-class type 'U'}}

/* Recursive superclass requirements on nominals and functions */

// Recursive generic signature validation.
class Top {}
class Bottom<T : Bottom<Top>> {}
// expected-error@-1 {{'Bottom' requires that 'Top' inherit from 'Bottom<Top>'}}
// expected-note@-2 {{requirement specified as 'T' : 'Bottom<Top>' [with T = Top]}}

class Bottom2<T: Bottom2<Top2>> {}
class Top2: Bottom2<Top2> {} // OK

class Bottom3<T: Bottom3<Top3, Top3, Top3>,
              U: Bottom3<Top3, Top3, Top3>,
              R: Bottom3<Top3, Top3, Top3>> {}
class Top3: Bottom3<Top3, Top3, Top3> {} // OK

// FIXME: Figure out how to work around resursive archetypes.
class FBoundedFIXME<T : FBoundedFIXME<T>> {}
// expected-error@-1 {{'FBoundedFIXME' requires that 'T' inherit from 'FBoundedFIXME<T>'}}
// expected-note@-2 {{requirement specified as 'T' : 'FBoundedFIXME<T>' [with T = T]}}

class GenericClass<T> {}

class RecSuperclassReqC<T: GenericClass<T>> {}
// expected-note@-1 {{requirement specified as 'T' : 'GenericClass<T>' [with T = GenericClass<Never>]}}
// expected-note@-2 {{requirement specified as 'T' : 'GenericClass<T>' [with T = T]}}
struct RecSuperclassReqS<T: GenericClass<T>> {}
// expected-note@-1 {{requirement specified as 'T' : 'GenericClass<T>' [with T = GenericClass<Never>]}}
// expected-note@-2 {{requirement specified as 'T' : 'GenericClass<T>' [with T = Never]}}
enum RecSuperclassReqE<T: GenericClass<T>> {}
// expected-note@-1 {{requirement specified as 'T' : 'GenericClass<T>' [with T = GenericClass<Never>]}}
typealias RecSuperclassReqT1<T: GenericClass<T>> = T
// expected-note@-1 {{requirement specified as 'T' : 'GenericClass<T>' [with T = GenericClass<Never>]}}

// FIXME: Figure out how to work around resursive archetypes.
typealias RecSuperclassReqT2<T: GenericClass<T>> = RecSuperclassReqC<T>
// expected-error@-1 {{'RecSuperclassReqC' requires that 'T' inherit from 'GenericClass<T>'}}

func recSuperclassReqF1<T: GenericClass<T>>(_: T.Type) {}
// expected-note@-1 {{where 'T' = 'Top', 'GenericClass<T>' = 'GenericClass<Top>'}}
func recSuperclassReqF2<T: GenericClass<U>, U: GenericClass<T>>(_: T.Type, _: U.Type) {}
// expected-note@-1 {{where 'T' = 'X', 'GenericClass<U>' = 'GenericClass<GenericClass<X>>'}}
func recSuperclassReqF3<T: GA<U>, U: GB<T>>(_: T.Type, _: U.Type) {}
// expected-note@-1 {{where 'U' = 'GA_Sub', 'GB<T>' = 'GB<GB_Sub>'}}

_ = RecSuperclassReqC<GenericClass<Never>>()
// expected-error@-1 {{'RecSuperclassReqC' requires that 'GenericClass<Never>' inherit from 'GenericClass<GenericClass<Never>>'}}
_ = RecSuperclassReqS<GenericClass<Never>>()
// expected-error@-1 {{'RecSuperclassReqS' requires that 'GenericClass<Never>' inherit from 'GenericClass<GenericClass<Never>>'}}
_ = RecSuperclassReqS<Never>()
// expected-error@-1 {{'RecSuperclassReqS' requires that 'Never' inherit from 'GenericClass<Never>'}}
_ = RecSuperclassReqE<GenericClass<Never>>.self
// expected-error@-1 {{'RecSuperclassReqE' requires that 'GenericClass<Never>' inherit from 'GenericClass<GenericClass<Never>>'}}
_ = RecSuperclassReqT1<GenericClass<Never>>.self
// expected-error@-1 {{'RecSuperclassReqT1' requires that 'GenericClass<Never>' inherit from 'GenericClass<GenericClass<Never>>'}}

class DerivedCRTP: GenericClass<DerivedCRTP> {}

_ = RecSuperclassReqC<DerivedCRTP>.self
_ = RecSuperclassReqS<DerivedCRTP>.self
_ = RecSuperclassReqE<DerivedCRTP>.self
_ = RecSuperclassReqT1<DerivedCRTP>.self

// Test functions
do {
  class X: GenericClass<Y> {}
  class Y: GenericClass<X> {}

  class GA_Sub: GA<GB_Sub> {}
  class GB_Sub: GB<GA_Sub> {}
  class GB_CRTP: GB<GB_CRTP> {}

  recSuperclassReqF1(Top.self)
  // expected-error@-1 {{global function 'recSuperclassReqF1' requires that 'Top' inherit from 'GenericClass<Top>'}}
  recSuperclassReqF2(X.self, GenericClass<X>.self)
  // expected-error@-1 {{global function 'recSuperclassReqF2' requires that 'X' inherit from 'GenericClass<GenericClass<X>>'}}
  recSuperclassReqF3(GB_Sub.self, GA_Sub.self)
  // expected-error@-1 {{global function 'recSuperclassReqF3' requires that 'GA_Sub' inherit from 'GB<GB_Sub>'}}

  recSuperclassReqF1(DerivedCRTP.self)
  recSuperclassReqF2(X.self, Y.self)
  recSuperclassReqF2(Y.self, X.self)
  recSuperclassReqF2(DerivedCRTP.self, DerivedCRTP.self)
  recSuperclassReqF3(GA_Sub.self, GB_Sub.self)
  recSuperclassReqF3(GB_CRTP.self, GB_CRTP.self)
}

/* Recursive superclass requirements in protocols */

protocol RecSuperclassReqP1: GenericClass<Self> {}
protocol RecSuperclassReqP2: GenericClass<Self> {
  associatedtype A: RecSuperclassReqP2 // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
}
protocol RecSuperclassReqP3: GenericClass<Self.A> {
  // FIXME: Infinite recursion.
  associatedtype A//: RecSuperclassReqP3
}
protocol RecSuperclassReqP4 {
  associatedtype A: GenericClass<A> // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
}

class InvalidConformanceToP1: GenericClass<Never>, RecSuperclassReqP1 {}
// expected-error@-1 {{'RecSuperclassReqP1' requires that 'InvalidConformanceToP1' inherit from 'GenericClass<InvalidConformanceToP1>'}}
// expected-note@-2 {{requirement specified as 'Self' : 'GenericClass<Self>' [with Self = InvalidConformanceToP1]}}
class InvalidConformanceToP2: GenericClass<InvalidConformanceToP2>,
                              RecSuperclassReqP2 {
// expected-error@-2 {{type 'InvalidConformanceToP2' does not conform to protocol 'RecSuperclassReqP2'}}
  typealias A = InvalidConformanceToP1
// expected-note@-1 {{possibly intended match 'InvalidConformanceToP2.A' (aka 'InvalidConformanceToP1') does not conform to 'RecSuperclassReqP2'}}
}
class InvalidConformanceToP3: GenericClass<Never>, RecSuperclassReqP3 {
// expected-error@-1 {{'RecSuperclassReqP3' requires that 'InvalidConformanceToP3' inherit from 'GenericClass<InvalidConformanceToP3.A>' (aka 'GenericClass<Bool>')}}
// expected-note@-2 {{requirement specified as 'Self' : 'GenericClass<Self.A>' [with Self = InvalidConformanceToP3]}}
  typealias A = Bool
}

class ConformanceToP1: GenericClass<ConformanceToP1>, RecSuperclassReqP1 {}
class ConformanceToP2P3: GenericClass<ConformanceToP2P3>,
                       RecSuperclassReqP2,
                       RecSuperclassReqP3 {
  typealias A = ConformanceToP2P3
}

// FIXME: False-negative. This bug impacts any associated type declarations
// that have a superclass bound containing dependent member types (see
// swift::checkTypeWitness).
struct ConformanceToP4: RecSuperclassReqP4 { // expected-error {{type 'ConformanceToP4' does not conform to protocol 'RecSuperclassReqP4'}}
  typealias A = DerivedCRTP // expected-note {{possibly intended match 'ConformanceToP4.A' (aka 'DerivedCRTP') does not inherit from 'GenericClass<τ_0_0.A>'}}
}

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

protocol P6: A, Other { } // expected-error {{protocol 'P6' cannot require 'Self' to be a subclass of both 'Other' and 'A'}}
// expected-error@-1{{multiple inheritance from classes 'A' and 'Other'}}
// expected-note@-2 {{superclass constraint 'Self' : 'A' written here}}

func takeA(_: A) { }
func takeP5<T: P5>(_ t: T) {
	takeA(t) // okay
}

protocol P7 {
	associatedtype Assoc: A, Other 
	// expected-note@-1{{superclass constraint 'Self.Assoc' : 'A' written here}}
	// expected-error@-2{{'Self.Assoc' cannot be a subclass of both 'Other' and 'A'}}
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
