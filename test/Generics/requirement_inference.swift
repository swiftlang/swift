// RUN: %target-parse-verify-swift -parse %s -verify
// RUN: %target-parse-verify-swift -parse -debug-generic-signatures %s > %t.dump 2>&1 
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
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : Fox [outer]
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
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : Canidae
func inferSuperclassRequirement1<T : Carnivora>(_ v: V<T>) {}

// CHECK-LABEL: .inferSuperclassRequirement2@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : Canidae
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
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : P3 [inferred @ {{.*}}:32]
// CHECK-NEXT:   U witness marker
// CHECK-NEXT:   U : P4 [inferred @ {{.*}}:32]
// CHECK-NEXT:   T[.P3].P3Assoc witness marker
// CHECK-NEXT:   T[.P3].P3Assoc : P1 [protocol @ {{.*}}:18]
// CHECK-NEXT:   T[.P3].P3Assoc : P2 [protocol @ {{.*}}:18]
// CHECK-NEXT:   U[.P4].P4Assoc == T[.P3].P3Assoc [inferred @ {{.*}}32]
func inferSameType1<T, U>(_ x: Model_P3_P4_Eq<T, U>) { }

// CHECK-LABEL: .inferSameType2@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : P3 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:25]
// CHECK-NEXT:   U witness marker
// CHECK-NEXT:   U : P4 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:33]
// CHECK-NEXT:   T[.P3].P3Assoc witness marker
// CHECK-NEXT:   T[.P3].P3Assoc : P1 [protocol @ {{.*}}requirement_inference.swift:{{.*}}:18]
// CHECK-NEXT:   T[.P3].P3Assoc : P2 [redundant @ {{.*}}requirement_inference.swift:{{.*}}:61]
// CHECK-NEXT:   U[.P4].P4Assoc == T[.P3].P3Assoc [explicit @ {{.*}}requirement_inference.swift:{{.*}}:75]
func inferSameType2<T : P3, U : P4>(_: T) where U.P4Assoc : P2, T.P3Assoc == U.P4Assoc {}

// CHECK-LABEL: .inferSameType3@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : PCommonAssoc1 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:25]
// CHECK-NEXT:   T : PCommonAssoc2 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:76]
// CHECK-NEXT:   T[.PCommonAssoc1].CommonAssoc witness marker
// CHECK-NEXT:   T[.PCommonAssoc1].CommonAssoc : P1 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:68]
// CHECK-NEXT:   T[.PCommonAssoc2].CommonAssoc == T[.PCommonAssoc1].CommonAssoc [inferred @ {{.*}}requirement_inference.swift:{{.*}}:76]
// CHECK-NEXT: Generic signature
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
// CHECK: Canonical generic signature for mangling: <τ_0_0 where τ_0_0 : P7, τ_0_0.AssocP6.Element : P6, τ_0_0.AssocP6.Element == τ_0_0.AssocP7.AssocP6.Element>
extension P7 where AssocP6.Element : P6, 
        AssocP7.AssocP6.Element : P6,
        AssocP6.Element == AssocP7.AssocP6.Element {
  func nestedSameType1() { }
}
