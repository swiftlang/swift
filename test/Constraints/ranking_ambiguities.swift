// RUN: %target-typecheck-verify-swift

// We have a special ranking rule that only currently applies to constructors,
// and compares the concrete parameter types.

protocol P {
  init()
}

// We currently only apply the constructor ranking rule to X() and not X.init().
struct S<T : P> {
  init(_ x: T = .init()) {} // expected-note {{found this candidate}}
  init(_ x: T? = nil) {} // expected-note {{found this candidate}}
  func testInitRanking() {
    _ = S<T>() // Okay
    _ = S<T>.init() // expected-error {{ambiguous use of 'init(_:)'}}
  }
}
struct S1 {
  init(x: Int = 0, y: Int = 0) {} // expected-note {{found this candidate}}
  init(_ x: Int = 0, _ y: Int = 0) {} // expected-note {{found this candidate}}

  func testInitRanking() {
    _ = S1() // Okay
    _ = S1.init() // expected-error {{ambiguous use of 'init'}}
  }
}
