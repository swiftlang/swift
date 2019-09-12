// RUN: %target-typecheck-verify-swift

typealias IntegerLiteralType = Int32

// Simple coercion of literals.
func simple() {
  _ = 1 as Int8
  _ = 1 as Int16
}


// Coercion of literals through operators.
func operators(_ x1: Int8) {
  let x2 : Int8 = 1 + 2
  let x3 : Int8 = 1 + x1
  _ = x2 + 1 as Int8
  _ = x1 + x2 + 1 + 4 + x3 + 5 as Int8
}

// Check coercion failure due to overflow.
struct X { }
struct Y { }
func accept_integer(_ x: Int8) -> X { } // expected-note 2{{found this candidate}}
func accept_integer(_ x: Int16) -> Y { } // expected-note 2{{found this candidate}}

func overflow_check() {
  var y : Y = accept_integer(500)

  accept_integer(17) // expected-error{{ambiguous use of 'accept_integer'}}
  accept_integer(1000000) // expected-error{{ambiguous use of 'accept_integer'}}
}

// Coercion chaining.
struct meters : ExpressibleByIntegerLiteral { 
  var value : Int8
  
  init(_ value: Int8) {
    self.value = value
  }

  typealias IntegerLiteralType = Int8
  init(integerLiteral value: Int8) {
    self.value = value
  }
}

struct supermeters : ExpressibleByIntegerLiteral { // expected-error{{type 'supermeters' does not conform to protocol 'ExpressibleByIntegerLiteral'}} expected-note {{do you want to add protocol stubs?}}
  var value : meters
  
  typealias IntegerLiteralType = meters // expected-note{{possibly intended match 'supermeters.IntegerLiteralType' (aka 'meters') does not conform to '_ExpressibleByBuiltinIntegerLiteral'}}
  init(_integerLiteral value: meters) {
    self.value = value
  }
}

func chaining() {
  var length : meters = 17
  // FIXME: missing truncation warning <rdar://problem/14070127>.
  var long_length : meters = 500
  var really_long_length : supermeters = 10
}

func memberaccess() {
  Int32(5._value) // expected-warning{{unused}}
  // This diagnostic is actually better than it looks, because the inner type is Builtin.Int32, not actually Int32.
  let x : Int32 = 7._value // expected-error{{cannot convert value of type 'Builtin.Int32' to specified type 'Swift.Int32'}}
  _ = x
}
