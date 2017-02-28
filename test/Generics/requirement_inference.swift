// RUN: %target-typecheck-verify-swift -typecheck %s -verify
// RUN: %target-typecheck-verify-swift -typecheck -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: %FileCheck %s < %t.dump

protocol P1 { 
  func p1()
}

protocol P2 : P1 { }


struct X1<T : P1> { 
  func getT() -> T { }
}

class X2<T : P1> {
  func getT() -> T { }
}

class X3 { }

struct X4<T : X3> { 
  func getT() -> T { }
}

struct X5<T : P2> { }

// Infer protocol requirements from the parameter type of a generic function.
func inferFromParameterType<T>(_ x: X1<T>) {
  x.getT().p1()
}

// Infer protocol requirements from the return type of a generic function.
func inferFromReturnType<T>(_ x: T) -> X1<T> {
  x.p1()
}

// Infer protocol requirements from the superclass of a generic parameter.
func inferFromSuperclass<T, U : X2<T>>(_ t: T, u: U) -> T {
  t.p1()
}


// Infer protocol requirements from the parameter type of a constructor.
struct InferFromConstructor {
  init<T> (x : X1<T>) {
    x.getT().p1()
  }
}


// Don't infer requirements for outer generic parameters.
class Fox : P1 {
  func p1() {}
}

class Box<T : Fox> {
// CHECK-LABEL: .unpack@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Fox [τ_0_0: Explicit]
  func unpack(_ x: X1<T>) {}
}

// ----------------------------------------------------------------------------
// Superclass requirements
// ----------------------------------------------------------------------------

// Compute meet of two superclass requirements correctly.
class Carnivora {}
class Canidae : Carnivora {}

struct U<T : Carnivora> {}

struct V<T : Canidae> {}

// CHECK-LABEL: .inferSuperclassRequirement1@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Canidae
func inferSuperclassRequirement1<T : Carnivora>(_ v: V<T>) {}

// CHECK-LABEL: .inferSuperclassRequirement2@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : Canidae
func inferSuperclassRequirement2<T : Canidae>(_ v: U<T>) {}

// ----------------------------------------------------------------------------
// Same-type requirements
// ----------------------------------------------------------------------------

protocol P3 {
  associatedtype P3Assoc : P2
}

protocol P4 {
  associatedtype P4Assoc : P1
}

protocol PCommonAssoc1 {
  associatedtype CommonAssoc
}

protocol PCommonAssoc2 {
  associatedtype CommonAssoc
}

protocol PAssoc {
  associatedtype Assoc
}

struct Model_P3_P4_Eq<T : P3, U : P4> where T.P3Assoc == U.P4Assoc {}

// CHECK-LABEL: .inferSameType1@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : P3 [τ_0_0: Inferred @ {{.*}}:32]
// CHECK-NEXT:   τ_0_1 : P4 [τ_0_1: Inferred @ {{.*}}:32]
// CHECK-NEXT:   τ_0_0[.P3].P3Assoc : P1 [τ_0_0: Inferred @ {{.*}}:32 -> Protocol requirement (P3) -> Protocol requirement (P2)]
// CHECK-NEXT:   τ_0_0[.P3].P3Assoc : P2 [τ_0_0: Inferred @ {{.*}}:32 -> Protocol requirement (P3)]
// FIXME: τ_0_0[.P3].P3Assoc == τ_0_1[.P4].P4Assoc [τ_0_0: Inferred]
func inferSameType1<T, U>(_ x: Model_P3_P4_Eq<T, U>) { }

// CHECK-LABEL: .inferSameType2@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : P3 [τ_0_0: Explicit @ {{.*}}:25]
// CHECK-NEXT:   τ_0_1 : P4 [τ_0_1: Explicit @ {{.*}}:33]
// CHECK-NEXT:   τ_0_0[.P3].P3Assoc : P1 [τ_0_0: Explicit @ {{.*}}:25 -> Protocol requirement (P3) -> Protocol requirement (P2)]
// CHECK-NEXT:   τ_0_0[.P3].P3Assoc : P2 [τ_0_0: Explicit @ {{.*}}:25 -> Protocol requirement (P3)]
// CHECK-NEXT:   τ_0_0[.P3].P3Assoc == τ_0_1[.P4].P4Assoc [τ_0_0[.P3].P3Assoc: Explicit]
func inferSameType2<T : P3, U : P4>(_: T, _: U) where U.P4Assoc : P2, T.P3Assoc == U.P4Assoc {}

// CHECK-LABEL: .inferSameType3@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   τ_0_0 : PCommonAssoc1 [τ_0_0: Explicit @ {{.*}}:25]
// CHECK-NEXT:   τ_0_0 : PCommonAssoc2 [τ_0_0: Explicit @ {{.*}}:74]
// CHECK-NEXT:   τ_0_0[.PCommonAssoc1].CommonAssoc : P1 [τ_0_0[.PCommonAssoc1].CommonAssoc: Explicit @ {{.*}}:66]
// CHECK-NEXT: Potential archetypes
func inferSameType3<T : PCommonAssoc1>(_: T) where T.CommonAssoc : P1, T : PCommonAssoc2 {}

protocol P5 {
  associatedtype Element
}

protocol P6 {
  associatedtype AssocP6 : P5
}

protocol P7 : P6 {
  associatedtype AssocP7: P6
}

// CHECK-LABEL: P7.nestedSameType1()@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : P7, τ_0_0.AssocP6.Element : P6, τ_0_0.AssocP6.Element == τ_0_0.AssocP7.AssocP6.Element>
extension P7 where AssocP6.Element : P6, 
        AssocP7.AssocP6.Element : P6,
        AssocP6.Element == AssocP7.AssocP6.Element {
  func nestedSameType1() { }
}

protocol P8 {
  associatedtype A
  associatedtype B
}

protocol P9 : P8 {
  associatedtype A
  associatedtype B
}

protocol P10 {
  associatedtype A
  associatedtype C
}

// CHECK-LABEL: sameTypeConcrete1@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : P10, τ_0_0 : P9, τ_0_0.A == X3, τ_0_0.B == Int, τ_0_0.C == Int>
func sameTypeConcrete1<T : P9 & P10>(_: T) where T.A == X3, T.C == T.B, T.C == Int { }

// CHECK-LABEL: sameTypeConcrete2@
// CHECK: Canonical generic signature: <τ_0_0 where τ_0_0 : P10, τ_0_0 : P9, τ_0_0.B == X3, τ_0_0.C == X3>
func sameTypeConcrete2<T : P9 & P10>(_: T) where T.B : X3, T.C == T.B, T.C == X3 { }

// Note: a standard-library-based stress test to make sure we don't inject
// any additional requirements.
// CHECK-LABEL: RangeReplaceableCollection.f()@
// CHECK: <τ_0_0 where τ_0_0 : MutableCollection, τ_0_0 : RangeReplaceableCollection, τ_0_0.SubSequence == MutableRangeReplaceableSlice<τ_0_0>>
extension RangeReplaceableCollection where
  Self: MutableCollection,
  Self.SubSequence == MutableRangeReplaceableSlice<Self>
{
	func f() { }
}

// CHECK-LABEL: X14.recursiveConcreteSameType
// CHECK: Generic signature: <T, V where T == CountableRange<Int>>
// CHECK-NEXT: Canonical generic signature: <τ_0_0, τ_1_0 where τ_0_0 == CountableRange<Int>>
struct X14<T: Collection> where T.Iterator == IndexingIterator<T> {
	func recursiveConcreteSameType<V>(_: V) where T == CountableRange<Int> { }
}

// rdar://problem/30478915
protocol P11 {
  associatedtype A
}

protocol P12 {
	associatedtype B: P11
}

struct X6 { }

struct X7 : P11 {
	typealias A = X6
}

struct X8 : P12 {
	typealias B = X7
}

struct X9<T: P12, U: P12> where T.B == U.B {
  // CHECK-LABEL: X9.upperSameTypeConstraint
	// CHECK: Generic signature: <T, U, V where U : P12, T == X8, U.B == X8.B>
  // CHECK: Canonical generic signature: <τ_0_0, τ_0_1, τ_1_0 where τ_0_1 : P12, τ_0_0 == X8, τ_0_1.B == X7>
	func upperSameTypeConstraint<V>(_: V) where T == X8 { }
}

protocol P13 {
	associatedtype C: P11
}

struct X10: P11, P12 {
	typealias A = X10
	typealias B = X10
}

struct X11<T: P12, U: P12> where T.B == U.B.A {
	// CHECK-LABEL: X11.upperSameTypeConstraint
	// CHECK: Generic signature: <T, U, V where T : P12, U == X10, T.B == X10.A>
	// CHECK: Canonical generic signature: <τ_0_0, τ_0_1, τ_1_0 where τ_0_0 : P12, τ_0_1 == X10, τ_0_0.B == X10>
	func upperSameTypeConstraint<V>(_: V) where U == X10 { }
}

#if _runtime(_ObjC)
// rdar://problem/30610428
@objc protocol P14 { }

class X12<S: AnyObject> {
  func bar<V>(v: V) where S == P14 {
  }
}

@objc protocol P15: P14 { }

class X13<S: P14> {
  func bar<V>(v: V) where S == P15 {
  }
}
#endif
