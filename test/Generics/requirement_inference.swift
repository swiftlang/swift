// RUN: %target-parse-verify-swift -parse %s -verify
// RUN: %target-parse-verify-swift -parse -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: FileCheck %s < %t.dump

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
func inferFromParameterType<T>(x: X1<T>) {
  x.getT().p1()
}

// Infer protocol requirements from the return type of a generic function.
func inferFromReturnType<T>(x: T) -> X1<T> {
  x.p1()
}

// Infer protocol requirements from the superclass of a generic parameter.
func inferFromSuperclass<T, U : X2<T>>(t: T, u: U) -> T {
  t.p1()
}


// Infer protocol requirements from the parameter type of a constructor.
struct InferFromConstructor {
  init<T> (x : X1<T>) {
    x.getT().p1()
  }
}


// FIXME: Infer superclass requirements.

// ----------------------------------------------------------------------------
// Same-type requirements
// ----------------------------------------------------------------------------

protocol P3 {
  typealias P3Assoc : P2
}

protocol P4 {
  typealias P4Assoc : P1
}

protocol PCommonAssoc1 {
  typealias CommonAssoc
}

protocol PCommonAssoc2 {
  typealias CommonAssoc
}

protocol PAssoc {
  typealias Assoc
}

struct Model_P3_P4_Eq<T : P3, U : P4 where T.P3Assoc == U.P4Assoc> { }

// CHECK-LABEL: .inferSameType1@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : P3 [inferred @ {{.*}}:26]
// CHECK-NEXT:   U witness marker
// CHECK-NEXT:   U : P4 [inferred @ {{.*}}:26]
// CHECK-NEXT:   T[.P3].P3Assoc witness marker
// CHECK-NEXT:   T[.P3].P3Assoc : P1 [protocol @ {{.*}}:26]
// CHECK-NEXT:   T[.P3].P3Assoc : P2 [protocol @ {{.*}}:26]
// CHECK-NEXT:   U[.P4].P4Assoc == T[.P3].P3Assoc [inferred @ {{.*}}26]
func inferSameType1<T, U>(x: Model_P3_P4_Eq<T, U>) { }

// CHECK-LABEL: .inferSameType2()@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : P3 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:21]
// CHECK-NEXT:   U witness marker
// CHECK-NEXT:   U : P4 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:29]
// CHECK-NEXT:   T[.P3].P3Assoc witness marker
// CHECK-NEXT:   T[.P3].P3Assoc : P1 [protocol @ {{.*}}requirement_inference.swift:{{.*}}:21]
// CHECK-NEXT:   T[.P3].P3Assoc : P2 [redundant @ {{.*}}requirement_inference.swift:{{.*}}:54]
// CHECK-NEXT:   U[.P4].P4Assoc == T[.P3].P3Assoc [explicit @ {{.*}}requirement_inference.swift:{{.*}}:68]
func inferSameType2<T : P3, U : P4 where U.P4Assoc : P2, T.P3Assoc == U.P4Assoc>() { }

// CHECK-LABEL: .inferSameType3()
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   T witness marker
// CHECK-NEXT:   T : PCommonAssoc1 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:21]
// CHECK-NEXT:   T : PCommonAssoc2 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:69]
// CHECK-NEXT:   T[.PCommonAssoc1].CommonAssoc witness marker
// CHECK-NEXT:   T[.PCommonAssoc1].CommonAssoc : P1 [explicit @ {{.*}}requirement_inference.swift:{{.*}}:61]
// CHECK-NEXT:   T[.PCommonAssoc2].CommonAssoc == T[.PCommonAssoc1].CommonAssoc [inferred @ {{.*}}requirement_inference.swift:{{.*}}:69]
// CHECK-NEXT: Generic signature
func inferSameType3<T : PCommonAssoc1 where T.CommonAssoc : P1, T : PCommonAssoc2>() { }
