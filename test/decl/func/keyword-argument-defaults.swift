// RUN: %swift -parse %s -verify

// Function arguments are not API by default, so this is okay.
func f1(a: Int, b: Int) { }
func f1(a a: Int, b b: Int) { } // okay: names differ

func f2(a: Int, b: Int) { } // expected-note{{'f2(::)' previously declared here}}
func f2(c: Int, d: Int) { } // expected-error{{invalid redeclaration of 'f2(::)'}}

class X {
  // Initializer arguments are API by default.
  init(x: Int) { }
  init(y: Int) { } // okay

  // Method arguments after the first are API by default
  func f1(a: Int, b: Int) { }
  func f1(a: Int, c: Int) { } // okay

  func f2(a: Int, b: Int) { } // expected-note{{'f2(:b:)' previously declared here}}
  func f2(A: Int, b: Int) { } // expected-error{{invalid redeclaration of 'f2(:b:)'}}
}

protocol P {
  func g1(value: Int)
  func g2(value: Int, other: Int)
  func g3(value: Int, other: Int, third: Int) // expected-note{{requirement 'g3(:other:third:)' declared here}}
  class func g4(value: Int)
}

class PX : P {
  func g1(x: Int) { } // okay
  func g2(x: Int, other: Int) { } // okay
  func g3(x: Int, y: Int, third: Int) { } // expected-error{{method 'g3(:y:third:)' has different argument names from those required by protocol 'P' ('g3(:other:third:)')}}

  class func g4(x: Int) { }
}
