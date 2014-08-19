// RUN: %swift -parse %s -verify

struct S0 {
  init!(int: Int) { }
  init! (uint: UInt) { }
  init !(float: Float) { }

  init?(string: String) { }
  init ?(double: Double) { }
  init ? (char: Character) { }
}

struct S1<T> {
  init?(value: T) { }
}

class DuplicateDecls {
  init!() { } // expected-note{{'init()' previously declared here}}
  init?() { } // expected-error{{invalid redeclaration of 'init()'}}

  init!(string: String) { } // expected-note{{'init(string:)' previously declared here}}
  init(string: String) { } // expected-error{{invalid redeclaration of 'init(string:)'}}

  init(double: Double) { } // expected-note{{'init(double:)' previously declared here}}
  init?(double: Double) { } // expected-error{{invalid redeclaration of 'init(double:)'}}
}

// Construct via a failable initializer.
func testConstruction(i: Int, s: String) {
  var s0Opt = S0(string: s)
  assert(s0Opt != nil)
  var s0: S0 = s0Opt // expected-error{{value of optional type 'S0?' not unwrapped; did you mean to use '!' or '?'?}}
  
  var s0IUO = S0(int: i)
  assert(s0IUO != nil)
  
  s0 = s0IUO
}
