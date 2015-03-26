// RUN: %target-parse-verify-swift

struct C1 {
  func =foo1(other: C1) {}
  func=foo2(other: C1) {} // expected-error{{expected space between 'func' and '='}}
  func = foo3(other: C1) {} // expected-error{{unexpected space within in-place method name}}

  func =foo4(other: C1) -> Int {} // expected-error{{in-place methods must return void}}
  static func =foo5(other: C1) {} // expected-error{{in-place methods may not be static}}
}

class C2 {
  func =foo2(other: C2) {} // expected-error{{only structs and enums may have in-place methods}}
}

func =foo2(inout foo: C1, bar: C2) {} // expected-error{{only structs and enums may have in-place methods}}

enum C3 {
  func =foo1(other: C3) {}
}

protocol P {
  func =foo1(other: Self)
}
extension C1: P {}

var c = C1()
c.=foo1(c)
c .= foo1(c)
c.=bogus(c) // expected-error{{'C1' does not have a member named '=bogus'}}
c .= bogus(c) // expected-error{{'C1' does not have a member named '=bogus'}}
c.=bogus // expected-error{{'C1' does not have a member named '=bogus'}}
c.=1 // expected-error{{expected identifier following '.='}}

let cc = C1()
cc.=foo1(c) // expected-error{{immutable value of type 'C1' only has mutating members named '=foo1'}}
cc .= foo1(c) // expected-error{{immutable value of type 'C1' only has mutating members named '=foo1'}}

infix operator ++++ { precedence 130 has_assignment }

func ++++(foo: C1, bar: C1) -> C1 { return foo }
func ++++=(inout foo: C1, bar: C1) { foo = foo ++++ bar }

struct Point {
  var x: Float
  var y: Float

  // Test that in-place method bodies are allowed to mutate object
  func =step(dx: Float, dy: Float) {
    x += dx
    y += dy
  }
}
