// RUN: %swift -parse %s -verify

// Simple struct types
struct X1 {
  static var AnX1: X1
  static var NotAnX1: Int
}

var x1: X1 = .AnX1
x1 = .AnX1
x1 = .NotAnX1 // expected-error{{'X1' is not identical to 'Int'}}

// Generic struct types
struct X2<T> {
  static var AnX2: X2 // FIXME: expected-error{{static variables not yet supported in generic types}}
  static var NotAnX2: Int // FIXME: expected-error{{static variables not yet supported in generic types}}
}

var x2: X2<Int> = .AnX2
x2 = .AnX2
x2 = .NotAnX2 // expected-error{{'X2<Int>' is not identical to 'Int'}}
