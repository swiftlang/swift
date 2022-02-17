// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/InferViaDefaults.swiftmodule -enable-experimental-type-inference-from-defaults -module-name InferViaDefaults %S/Inputs/type_inference_via_defaults_other_module.swift
// RUN: %target-swift-frontend -enable-experimental-type-inference-from-defaults -module-name main -typecheck -verify -I %t %s %S/Inputs/type_inference_via_defaults_other_module.swift

func testInferFromResult<T>(_: T = 42) -> T { fatalError() }
// expected-error@-1 {{cannot use default expression for inference of 'T' because it is inferrable from result type}}

func testInferFromOtherPos1<T>(_: T = 42, _: [T]) {}
// expected-error@-1 {{cannot use default expression for inference of 'T' because it is inferrable from parameters #0, #1}}

func testInferFromOtherPos2<T>(_: T = 42, _: T = 0.0) {}
// expected-error@-1 2 {{cannot use default expression for inference of 'T' because it is inferrable from parameters #0, #1}}

protocol P {
  associatedtype X
}

func testInferFromSameType<T, U: P>(_: T = 42, _: [U]) where T == U.X {}
// expected-error@-1 {{cannot use default expression for inference of 'T' because it is inferrable through same-type requirement: T == U.X}}

func test1<T>(_: T = 42) {} // Ok

struct S : P {
  typealias X = Int
}

func test2<T: P>(_: T = S()) {} // Ok

struct A : P {
  typealias X = Double
}

class B : P {
  typealias X = String

  init() {}
}

func test2<T: P & AnyObject>(_: T = B()) {} // Ok

func test2NonClassDefault<T: P & AnyObject>(_: T = S()) {}
// expected-error@-1 {{global function 'test2NonClassDefault' requires that 'S' be a class type}}
// expected-note@-2 {{where 'T' = 'S'}}

func test2NonConformingDefault<T: P>(_: T = 42.0) {}
// expected-error@-1 {{global function 'test2NonConformingDefault' requires that 'Double' conform to 'P'}}
// expected-note@-2 {{where 'T' = 'Double'}}

func testMultiple<T, U>(a: T = 42.0, b: U = "") {} // Ok

// Subscripts

extension S {
  subscript<T: P>(a: T = S()) -> Int {
    get { return 42 }
  }

  subscript<T: P, U: AnyObject>(a: T = S(), b: U = B()) -> Int {
    get { return 42 }
  }
}


func main() {
  test1() // Ok

  test2() // Ok
  test2(A()) // Ok as well

  testMultiple()                // Ok (T = Double, U = String)
  testMultiple(a: 0)            // Ok (T = Int, U = String)
  testMultiple(b: S())          // Ok (T = Double, U = S)
  testMultiple(a: 0.0, b: "a")  // Ok

  // From a different module
  with_defaults() // Ok
  with_defaults("") // Ok

  _ = S()[] // Ok
  _ = S()[B()] // Ok
}
