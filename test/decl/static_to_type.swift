// RUN: %swift -parse %s -verify

struct X {
  static func f() { } // expected-error{{the 'static' keyword has been renamed to 'type'}}{5-11=type}}
  static var x: Int // expected-error{{the 'static' keyword has been renamed to 'type'}}{5-11=type}}
}
