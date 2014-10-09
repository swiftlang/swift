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

// FIXME: Infer same-type requirements, which requires us to process
// the where clause in its entirety.

// ----------------------------------------------------------------------------
// Redundant requirements
// ----------------------------------------------------------------------------

// CHECK-LABEL: .redundant1()@
// CHECK-NEXT: Archetypes to build:
// CHECK-NEXT: T : protocol<P2 [explicit @ {{.*}}requirement_inference.swift:[[@LINE+1]]:29], P1 [protocol @ {{.*}}requirement_inference.swift:[[@LINE+1]]:29]>
func redundant1<T where T : P2, T : P1>() { }
func redundant2<T where T : P1, T : P2>() { }
func redundant3<T where T : P2>(x: X5<T>) { }
func redundant4<T where T : P1>(x: X5<T>) { }

func redundant5<T : X3 where T : X3>() { }
func redundant6<T : X3>(x: X4<T>) { }
