// RUN: %target-typecheck-verify-swift

enum E : String {
  case foo = "foo"
  case bar = "bar" // expected-note {{'bar' declared here}}
}
func fe(_: E) {}
func fe(_: Int) {}
func fe(_: Int, _: E) {}
func fe(_: Int, _: Int) {}

fe(E.baz) // expected-error {{type 'E' has no member 'baz'; did you mean 'bar'?}}
fe(.baz) // expected-error {{reference to member 'baz' cannot be resolved without a contextual type}}

// FIXME: maybe complain about .nope also?
fe(.nope, .nyet) // expected-error {{reference to member 'nyet' cannot be resolved without a contextual type}}

func fg<T>(_ f: (T) -> T) -> Void {} // expected-note {{in call to function 'fg'}}
fg({x in x}) // expected-error {{generic parameter 'T' could not be inferred}}


// FIXME: Both f & g should complain about ambiguity of arg1, but in both cases the generic member
// never gets into the list of candidates in the first place.
struct S {
  func f<T>(_ i: (T) -> T, _ j: Int) -> Void {}
  func f(_ d: (Double) -> Double) -> Void {}
  func test() -> Void {
    f({x in x}, 2) // expected-error {{extra argument in call}}
  }
  
  func g<T>(_ a: T, _ b: Int) -> Void {}
  func g(_ a: String) -> Void {}
  func test2() -> Void {
    g(.notAThing, 7) // expected-error {{extra argument in call}}
  }
  
  func h(_ a: Int, _ b: Int) -> Void {}
  func h(_ a: String) -> Void {}
  func test3() -> Void {
    h(.notAThing, 3) // expected-error {{type 'Int' has no member 'notAThing'}}
    h(.notAThing) // expected-error {{type 'String' has no member 'notAThing'}}
  }
}

