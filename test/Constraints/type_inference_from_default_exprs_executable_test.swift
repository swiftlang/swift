// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/InferViaDefaults.swiftmodule -module-name InferViaDefaults %S/Inputs/type_inference_via_defaults_other_module.swift
// RUN: %target-build-swift -module-name main -Xfrontend -parse-as-library -I %t %s %S/Inputs/type_inference_via_defaults_other_module.swift -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: OS=macosx && CPU=x86_64

protocol P {
  associatedtype X
}

func test1<T>(_ v: T = 42) {
  print("test1: \(v); type = \(type(of: v))")
}

struct S : P {
  typealias X = Int
}

func test2<T: P>(_ v: T = S()) {
  print("test2: \(v); type = \(type(of: v))")
}

struct A : P {
  typealias X = Double
}

class B : P {
  typealias X = String

  init() {}
}

class C {
  init() {}
}

func test2<T: P & AnyObject>(_ v: T = B()) {
  print("test2(object): \(v); type = \(type(of: v))")
}

func testMultiple<T, U>(a: T = 42.0, b: U = "hello") {
  print("testMultiple: a = \(a); type = \(type(of: a))")
  print("testMultiple: b = \(b); type = \(type(of: b))")
}

// Subscripts

extension S {
  subscript<T: P>(a: T = S()) -> Int {
    get {
      print("subscript(a): a = \(a); type = \(type(of: a))")
      return 42
    }
  }

  subscript<T: P, U: AnyObject>(a a: T = S(), b: U = B()) -> Int {
    get {
      print("subscript(a, b): a = \(a); type = \(type(of: a))")
      print("subscript(a, b): b = \(b); type = \(type(of: b))")
      return 42
    }
  }
}

func testNested1<T>(_ v: [T] = [0, 1.0]) {
  print("testNested1: \(v); type = \(type(of: v))")
}
func testNested2<T>(_ v: T? = 42.0) {
  print("testNested2: \(v); type = \(type(of: v))")
}

struct D : P {
  typealias X = B
}

func testNested3<T: P>(_ v: T = B()) where T.X == String {
  print("testNested3: \(v); type = \(type(of: v))")
}
func testNested4<T: P>(_ v: [T]? = [D()]) where T.X: P, T.X: AnyObject {
  print("testNested4: \(v); type = \(type(of: v))")
}
func testNested5<T: P, U>(_ v: (a: [T?], b: U) = (a: [D()], b: B())) where T.X == U, T.X: P, U: AnyObject {
  print("testNested5: \(v); type = \(type(of: v))")
}

class GenClass<T> {}

class E : GenClass<B> {
}

func testReq1<T, U>(_ v: (T, U) = (E(), B())) where T: GenClass<U>, U: AnyObject {
  print("testReq1: \(v); type = \(type(of: v))")
}

class OtherB : P {
  typealias X = String
}

struct OtherD : P {
  typealias X = OtherB
}

protocol Shape {
}

struct Circle : Shape {
}

struct Rectangle : Shape {
}

struct Figure<S: Shape> {
  init(v: S = Circle()) {
    print("Figure: \(v); type = \(type(of: v))")
  }
}

enum TestE<T: Collection> {
case a(_: T = [B()])
}

func run_tests() {
  test1()
  // CHECK: test1: 42; type = Int
  test1("hello")
  // CHECK: test1: hello; type = String

  test2(A())
  // CHECK: test2: A(); type = A
  test2()
  // CHECK: test2(object): main.B; type = B

  testMultiple()
  // CHECK: testMultiple: a = 42.0; type = Double
  // CHECK-NEXT: testMultiple: b = hello; type = String
  testMultiple(a: 0)
  // CHECK: testMultiple: a = 0; type = Int
  // CHECK-NEXT: testMultiple: b = hello; type = String
  testMultiple(b: S())
  // CHECK: testMultiple: a = 42.0; type = Double
  // CHECK-NEXT: testMultiple: b = S(); type = S
  testMultiple(a: "a", b: 42.0 as Float)
  // CHECK: testMultiple: a = a; type = String
  // CHECK-NEXT: testMultiple: b = 42.0; type = Float

  // From a different module
  with_defaults()
  // CHECK: with_defaults: 42.0; type = Double
  with_defaults("str")
  // CHECK: with_defaults: str; type = String

  _ = S()[] // Ok
  // CHECK: subscript(a): a = S(); type = S
  _ = S()[B()] // Ok
  // CHECK: subscript(a): a = main.B; type = B

  _ = S()[a: B()] // Ok
  // CHECK: subscript(a, b): a = main.B; type = B
  // CHECK-NEXT: subscript(a, b): b = main.B; type = B

  testNested1()
  // CHECK: testNested1: [0.0, 1.0]; type = Array<Double>
  testNested1(["a", "b"])
  // CHECK: testNested1: ["a", "b"]; type = Array<String>
  testNested2()
  // CHECK: testNested2: Optional(42.0); type = Optional<Double>
  testNested2(nil as Float?)
  // CHECK: testNested2: nil; type = Optional<Float>
  testNested3()
  // CHECK: testNested3: main.B; type = B
  testNested3(OtherB())
  // CHECK: testNested3: main.OtherB; type = OtherB
  testNested4()
  // CHECK: testNested4: Optional([main.D()]); type = Optional<Array<D>>
  testNested4([OtherD()])
  // CHECK: testNested4: Optional([main.OtherD()]); type = Optional<Array<OtherD>>
  testNested5()
  // CHECK: testNested5: (a: [Optional(main.D())], b: main.B); type = (a: Array<Optional<D>>, b: B)

  testReq1()
  // CHECK: testReq1: (main.E, main.B); type = (E, B)

  _ = Figure.init()
  // CHECK: Figure: Circle(); type = Circle
  let _: Figure<Circle> = .init()
  // CHECK: Figure: Circle(); type = Circle

  func takesFigure<T>(_: Figure<T>) {}

  takesFigure(.init())
  // CHECK: Figure: Circle(); type = Circle

  let e: TestE = .a()
  print("ETEst: \(e); type = \(type(of: e))")
  // CHECK: ETEst: a([main.B]); type = TestE<Array<B>>
}

@main struct Test {
  static func main() {
    run_tests()
  }
}
