// RUN: %target-parse-verify-swift

// Simple struct types
struct X1 {
  static var AnX1 = X1()
  static var NotAnX1 = 42
}

func acceptInOutX1(inout x1: X1) { }

var x1: X1 = .AnX1
x1 = .AnX1
x1 = .NotAnX1 // expected-error{{could not find member 'NotAnX1'}}

// Delayed identifier expressions as lvalues
(.AnX1 = x1)
acceptInOutX1(&(.AnX1))

// Generic struct types
struct X2<T> {
  static var AnX2 = X2() // expected-error{{argument for generic parameter 'T' could not be inferred}}
  static var NotAnX2 = 0 // expected-error {{static stored properties not yet supported in generic types}}
}

var x2: X2<Int> = .AnX2  // expected-error {{'X2<Int>.Type' does not have a member named 'AnX2'}}
x2 = .AnX2     // expected-error {{'X2<Int>.Type' does not have a member named 'AnX2'}}
x2 = .NotAnX2 // expected-error{{could not find member 'NotAnX2'}}

// Static variables through operators.
struct Foo {
  static var Bar = Foo()
  static var Wibble = Foo()
}

func & (x: Foo, y: Foo) -> Foo { }

var fooValue: Foo = .Bar & .Wibble
