// RUN: %swift -parse %s -verify
// RUN: %swift -parse -debug-generic-signatures %s > %t.dump 2>&1 
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

struct Model_P3_P4_Eq<T : P3, U : P4 where T.P3Assoc == U.P4Assoc> { }

// CHECK-LABEL: .inferSameType1@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   U : P4 [inferred @ {{.*}}:[[@LINE+5]]:26]
// CHECK-NEXT:   U[.P4].P4Assoc == T.P3Assoc [inferred @ {{.*}}:[[@LINE+4]]:26]
// CHECK-NEXT:   T : P3 [inferred @ {{.*}}:[[@LINE+3]]:26]
// CHECK-NEXT:   T[.P3].P3Assoc : P2 [protocol @ {{.*}}:[[@LINE+2]]:26]
// CHECK-NEXT:   T[.P3].P3Assoc : P1 [protocol @ {{.*}}:[[@LINE+1]]:26]
func inferSameType1<T, U>(x: Model_P3_P4_Eq<T, U>) { }

// CHECK-LABEL: .inferSameType2()@
// CHECK-NEXT: Requirements:
// CHECK-NEXT:   U : P4 [explicit @ {{.*}}requirement_inference.swift:[[@LINE+5]]:29]
// CHECK-NEXT:   U[.P4].P4Assoc == T.P3Assoc [explicit @ {{.*}}requirement_inference.swift:[[@LINE+4]]:68]
// CHECK-NEXT:   T : P3 [explicit @ {{.*}}requirement_inference.swift:[[@LINE+3]]:21]
// CHECK-NEXT:   T[.P3].P3Assoc : P2 [protocol @ {{.*}}requirement_inference.swift:[[@LINE+2]]:21]
// CHECK-NEXT:   T[.P3].P3Assoc : P1 [protocol @ {{.*}}requirement_inference.swift:[[@LINE+1]]:21]
func inferSameType2<T : P3, U : P4 where U.P4Assoc : P2, T.P3Assoc == U.P4Assoc>() { }
