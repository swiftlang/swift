// RUN: %target-typecheck-verify-swift -swift-version 5

func f0(_ i: Int, _ d: Double) {} // expected-note{{found this candidate}}
func f0(_ d: Double, _ i: Int) {} // expected-note{{found this candidate}}

f0(1, 2) // expected-error{{ambiguous use of 'f0', cannot select between potential parameter types '(Double, Int)', '(Int, Double)'}}

func f1(_ i: Int16) {} // expected-note{{found this candidate}}
func f1(_ i: Int32) {} // expected-note{{found this candidate}}

f1(0) // expected-error{{ambiguous use of 'f1', cannot select between potential parameter types '(Int16)', '(Int32)'}}

infix operator +++

func +++(i: Int, d: Double) {} // expected-note{{found this candidate}}
func +++(d: Double, i: Int) {} // expected-note{{found this candidate}}

1 +++ 2 // expected-error{{ambiguous use of operator '+++', cannot select between potential parameter types '(Double, Int)', '(Int, Double)'}}

class C {
  init(_ action: (Int) -> ()) {} 
  init(_ action: (Int, Int) -> ()) {} 
}

func g(_ x: Int) -> () {} // expected-note{{found this candidate}}
func g(_ x: Int, _ y: Int) -> () {} // expected-note{{found this candidate}}
C(g) // expected-error{{ambiguous use of 'g', cannot select between potential parameter types '(Int)', '(Int, Int)'}}

func h<T>(_ x: T) -> () {}
_ = C(h) // OK - init(_: (Int) -> ())

func rdar29691909_callee(_ o: AnyObject?) -> Any? { return o } // expected-note {{found this candidate}}
func rdar29691909_callee(_ o: AnyObject) -> Any { return o } // expected-note {{found this candidate}}

func rdar29691909(o: AnyObject) -> Any? {
  return rdar29691909_callee(o) // expected-error{{ambiguous use of 'rdar29691909_callee', cannot select between potential result types 'Any', 'Any?'}}
}

func rdar29907555(_ value: Any!) -> String {
  return "\(value)" // expected-warning {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-1 {{use a default value parameter to avoid this warning}}
  // expected-note@-2 {{provide a default value to avoid this warning}}
  // expected-note@-3 {{use 'String(describing:)' to silence this warning}}
}

// https://github.com/apple/swift/issues/46300
struct S_46300 {
  var overloaded: Int! // expected-note {{implicitly unwrapped property 'overloaded' declared here}}

  func overloaded(_ x: Int) {}
  func overloaded(_ x: Float) {}

  func take(_ a: [Any]) {}

  func test() {
    take([overloaded])
    // expected-warning@-1 {{coercion of implicitly unwrappable value of type 'Int?' to 'Any' does not unwrap optional}}
    // expected-note@-2 {{provide a default value to avoid this warning}}
    // expected-note@-3 {{force-unwrap the value to avoid this warning}}
    // expected-note@-4 {{explicitly cast to 'Any' with 'as Any' to silence this warning}}
  }
}

// rdar://35116378 - Here the ambiguity is in the pre-check pass; make sure
// we emit a diagnostic instead of crashing.
struct Movie {}

class MoviesViewController {
  typealias itemType = Movie // expected-note {{'itemType' declared here}}
  let itemType = [Movie].self // expected-note {{'itemType' declared here}}
  var items: [Movie] = [Movie]()

  func loadData() {
    _ = itemType // expected-error {{ambiguous use of 'itemType'}}
  }
}

// https://github.com/apple/swift/issues/57380

func f1_57380<T : Numeric>(_ a: T, _ b: T) -> T {
  (a + b) / 2 // expected-note {{overloads for '/' exist with these partially matching parameter lists: (Int, Int)}}
  // expected-error@-1 {{binary operator '/' cannot be applied to operands of type 'T' and 'Int'}}
}

infix operator %%

func %% (_ lhs: Int, _ rhs: Int) -> Int {
  lhs / rhs
}

func %% (_ lhs: Float, _ rhs: Float) -> Float {
  lhs / rhs
}

func f2_57380<T : Numeric>(_ a: T, _ b: T) {
  (a + b) %% 2 // expected-error {{cannot convert value of type 'T' to expected argument type 'Int'}}
}

// rdar://94360230 - diagnosing 'filter' instead of ambiguity in its body
func test_diagnose_deepest_ambiguity() {
  struct S {
    func ambiguous(_: Int = 0) -> Bool { true }     // expected-note 2 {{found this candidate}}
    func ambiguous(_: String = "") -> Bool { true } // expected-note 2 {{found this candidate}}
  }

  func test_single(arr: [S]) {
    arr.filter { $0.ambiguous() } // expected-error {{ambiguous use of 'ambiguous', cannot select between potential parameter types '(Int)', '(String)'}}
  }

  func test_multi(arr: [S]) {
    arr.filter {
      if true {
        print($0.ambiguous()) // expected-error {{ambiguous use of 'ambiguous', cannot select between potential parameter types '(Int)', '(String)'}}
      }
      return true
    }
  }
}

// FIXME: `(Int, Int) -> Int` seems like it would be a better match, also we ought
// to only emit this diagnostic once (https://github.com/swiftlang/swift/issues/88910).
let unappliedResultMismatch: (Int, Int) -> Double = (+)
// expected-error@-1 2{{cannot convert value of type '(Double) -> Double' to specified type '(Int, Int) -> Double'}}

protocol P {}
protocol Q {}

extension P {
  func foo() {} // expected-note {{found this candidate}}
}
extension Q {
  func foo() {} // expected-note {{found this candidate}}
}
struct S: P, Q {}

func bar(_ x: S) {
  x.foo() // expected-error {{ambiguous use of 'foo()'}}
}

protocol P2 {}
protocol Q2 {}

extension P2 {
  func foo() -> Int { 0 }
}
extension Q2 {
  func foo() -> Int { 0 }
}
struct S2: P2, Q2 {}

func bar(_ x: S2) {
  x.foo() + x.foo() // expected-error {{failed to produce diagnostic}}
}
