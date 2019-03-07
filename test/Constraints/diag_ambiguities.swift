// RUN: %target-typecheck-verify-swift

func f0(_ i: Int, _ d: Double) {} // expected-note{{found this candidate}}
func f0(_ d: Double, _ i: Int) {} // expected-note{{found this candidate}}

f0(1, 2) // expected-error{{ambiguous use of 'f0'}}

func f1(_ i: Int16) {} // expected-note{{found this candidate}}
func f1(_ i: Int32) {} // expected-note{{found this candidate}}

f1(0) // expected-error{{ambiguous use of 'f1'}}

infix operator +++

func +++(i: Int, d: Double) {} // expected-note{{found this candidate}}
func +++(d: Double, i: Int) {} // expected-note{{found this candidate}}

1 +++ 2 // expected-error{{ambiguous use of operator '+++'}}

class C {
  init(_ action: (Int) -> ()) {} // expected-note{{found this candidate}}
  init(_ action: (Int, Int) -> ()) {} // expected-note{{found this candidate}}
}

func g(_ x: Int) -> () {} // expected-note{{found this candidate}}
func g(_ x: Int, _ y: Int) -> () {} // expected-note{{found this candidate}}
C(g) // expected-error{{ambiguous use of 'g'}}

func h<T>(_ x: T) -> () {}
C(h) // expected-error{{ambiguous use of 'init(_:)'}}

func rdar29691909_callee(_ o: AnyObject?) -> Any? { return o } // expected-note {{found this candidate}}
func rdar29691909_callee(_ o: AnyObject) -> Any { return o } // expected-note {{found this candidate}}

func rdar29691909(o: AnyObject) -> Any? {
  return rdar29691909_callee(o) // expected-error{{ambiguous use of 'rdar29691909_callee'}}
}

func rdar29907555(_ value: Any!) -> String {
  return "\(value)" // expected-warning {{string interpolation produces a debug description for an optional value; did you mean to make this explicit?}}
  // expected-note@-1 {{use 'String(describing:)' to silence this warning}}
  // expected-note@-2 {{provide a default value to avoid this warning}}
}

struct SR3715 {
  var overloaded: Int!

  func overloaded(_ x: Int) {}
  func overloaded(_ x: Float) {}

  func take(_ a: [Any]) {}

  func test() {
    take([overloaded])
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
