// RUN: %target-typecheck-verify-swift

protocol P0 {
  // expected-note @+1 {{protocol requires function 'callAsFunction()' with type '() -> Missing'}}
  func callAsFunction() -> Self
}
func testProtocol(_ x: P0) {
  _ = x()
}
func testGeneric<T : P0>(_ x: T) {
  _ = x()
}

protocol P1 {
  func callAsFunction() -> Self
}
extension P1 {
  // expected-note @+1 {{found this candidate}}
  func callAsFunction() -> Self {
    return self
  }
}
protocol P2 {}
extension P2 {
  // expected-note @+1 {{found this candidate}}
  func callAsFunction(x: Int, y: Int) -> Int {
    return x + y
  }
}

// expected-note@+2 {{add stubs for conformance}}
// expected-error @+1 {{type 'Missing' does not conform to protocol 'P0'}}
struct Missing : P0 {}
struct S0 : P0 {
  @discardableResult
  func callAsFunction() -> S0 { return self }
}
let s0 = S0()
s0()

struct S1 : P1 {
  func callAsFunction() -> S1 { return self }
}

let s1 = S1()
_ = s1()()

struct Conforming : P0 & P1 & P2 {}
let conforming = Conforming()
_ = conforming(x: 1, y: 2)
_ = conforming().callAsFunction(x:y:)(1, 2)
_ = conforming.callAsFunction(x:y:)
_ = conforming.callAsFunction // expected-error {{ambiguous use of 'callAsFunction'}}

protocol P3 {}
extension P3 {
  func callAsFunction() -> Self { return self }
}
struct S3 : P3 {}

let s3 = S3()
_ = s3()()
