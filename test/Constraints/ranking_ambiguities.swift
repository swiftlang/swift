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

// Ambiguous because we don't prefer one label over the other.
struct S2 {
  init(x: Int...) {} // expected-note {{found this candidate}}
  init(y: Int...) {} // expected-note {{found this candidate}}

  func testInitRanking() {
    _ = S2() // expected-error {{ambiguous use of 'init'}}
  }
}

// Ambiguous because we don't apply the prefer-unlabeled rule if the types
// aren't compatible.
struct S3 {
  init(x: Int...) {} // expected-note {{found this candidate}}
  init(_: String...) {} // expected-note {{found this candidate}}

  func testInitRanking() {
    _ = S3() // expected-error {{ambiguous use of 'init'}}
  }
}

// Ambiguous because this ends up being a comparison between a paren and tuple
// parameter list, and we don't have a special case for it. Ideally we would
// align this behavior with the variadic behavior.
struct S4 {
  init(x: Int = 0) {} // expected-note {{found this candidate}}
  init(_: Int = 0) {} // expected-note {{found this candidate}}

  func testInitRanking() {
    _ = S4() // expected-error {{ambiguous use of 'init'}}
  }
}

infix operator ^^^
func ^^^ (lhs: (Int, Int), rhs: Int) -> Int { 0 }  // expected-note {{found this candidate}}
func ^^^ (lhs: (Int, Int), rhs: Int) -> String { "" }  // expected-note {{found this candidate}}

// We shouldn't favor based on the type of a tuple element.
struct S5 {
  init(_ x: Int) {}
  init(_ x: String) {}

  func testFavoring() {
    let x = 0
    _ = S5((x, 0) ^^^ 0) // expected-error {{ambiguous use of operator '^^^'}}
  }
}
