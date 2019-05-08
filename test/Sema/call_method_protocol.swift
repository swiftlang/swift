// RUN: %target-typecheck-verify-swift

protocol P0 {
  // expected-note @+1 {{protocol requires function 'call()' with type '() -> Missing'; do you want to add a stub?}}
  func call() -> Self
}
func testProtocol(_ x: P0) {
  _ = x()
}
func testGeneric<T : P0>(_ x: T) {
  _ = x()
}

protocol P1 {
  func call() -> Self
}
extension P1 {
  // expected-note @+1 {{found this candidate}}
  func call() -> Self {
    return self
  }
}
protocol P2 {}
extension P2 {
  // expected-note @+1 {{found this candidate}}
  func call(x: Int, y: Int) -> Int {
    return x + y
  }
}

// expected-error @+1 {{type 'Missing' does not conform to protocol 'P0'}}
struct Missing : P0 {}
struct S0 : P0 {
  @discardableResult
  func call() -> S0 { return self }
}
let s0 = S0()
s0()

struct S1 : P1 {
  func call() -> S1 { return self }
}

let s1 = S1()
_ = s1()()

struct Conforming : P0 & P1 & P2 {}
let conforming = Conforming()
_ = conforming(x: 1, y: 2)
_ = conforming().call(x:y:)(1, 2)
_ = conforming.call(x:y:)
_ = conforming.call // expected-error {{ambiguous use of 'call'}}

protocol P3 {}
extension P3 {
  func call() -> Self { return self }
}
struct S3 : P3 {}

let s3 = S3()
_ = s3()()
