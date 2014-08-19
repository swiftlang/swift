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
