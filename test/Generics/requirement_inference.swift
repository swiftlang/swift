// RUN: %swift -parse %s -verify

protocol P1 { 
  func p1()
}

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

// Infer protocol requirements from the parameter type of a generic function.
func inferFromParameterType<T>(x : X1<T>) {
  x.getT().p1()
}

// Infer protocol requirements from the return type of a generic function.
func inferFromReturnType<T>(x : T) -> X1<T> {
  x.p1()
}

// Infer protocol requirements from the superclass of a generic parameter.
func inferFromSuperclass<T, U : X2<T>>(t : T, u : U) -> T {
  t.p1()
}


// Infer protocol requirements from the parameter type of a constructor.
struct InferFromConstructor {
  constructor<T> (x : X1<T>) {
    x.getT().p1()
  }
}

// FIXME: Infer superclass requirements.

// FIXME: Infer same-type requirements, which requires us to process
// the where clause in its entirety.
