// RUN: %target-typecheck-verify-swift

// Simple struct types
struct X1 {
  static var AnX1 = X1()
  static var NotAnX1 = 42
}

func acceptInOutX1(_ x1: inout X1) { }

var x1: X1 = .AnX1
x1 = .AnX1
x1 = .NotAnX1 // expected-error{{member 'NotAnX1' in 'X1' produces result of type 'Int', but context expects 'X1'}}

// Delayed identifier expressions as lvalues
(.AnX1 = x1)
acceptInOutX1(&(.AnX1))

// Generic struct types
struct X2<T> {
  static var AnX2 = X2() // expected-error{{static stored properties not supported in generic types}}
  static var NotAnX2 = 0 // expected-error {{static stored properties not supported in generic types}}
}

var x2: X2<Int> = .AnX2 
x2 = .AnX2    // reference to isInvalid() decl.
x2 = .NotAnX2 // expected-error{{member 'NotAnX2' in 'X2<Int>' produces result of type 'Int', but context expects 'X2<Int>'}}

// Static variables through operators.
struct Foo {
  static var Bar = Foo()
  static var Wibble = Foo()
}

func & (x: Foo, y: Foo) -> Foo { }

var fooValue: Foo = .Bar & .Wibble

// Static closure variables.
struct HasClosure {
  static var factoryNormal: (Int) -> HasClosure = { _ in .init() }
  static var factoryReturnOpt: (Int) -> HasClosure? = { _ in .init() }
  static var factoryIUO: ((Int) -> HasClosure)! = { _ in .init() }
  static var factoryOpt: ((Int) -> HasClosure)? = { _ in .init() }
}
var _: HasClosure = .factoryNormal(0)
var _: HasClosure = .factoryReturnOpt(1)!
var _: HasClosure = .factoryIUO(2)
var _: HasClosure = .factoryOpt(3) // expected-error {{static property 'factoryOpt' is not a function}}
var _: HasClosure = .factoryOpt!(4) // expected-error {{type of expression is ambiguous without more context}}
