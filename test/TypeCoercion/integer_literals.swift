// RUN: %target-parse-verify-swift

typealias IntegerLiteralType = Int32

// Simple coercion of literals.
func simple() {
  _ = 1 as Int8
  _ = 1 as Int16
}


// Coercion of literals through operators.
func operators(x1: Int8) {
  let x2 : Int8 = 1 + 2
  let x3 : Int8 = 1 + x1
  _ = x2 + 1 as Int8
  _ = x1 + x2 + 1 + 4 + x3 + 5 as Int8
}

// Check coercion failure due to overflow.
struct X { }
struct Y { }
func accept_integer(x: Int8) -> X { } // expected-note 2{{found this candidate}}
func accept_integer(x: Int16) -> Y { } // expected-note 2{{found this candidate}}

func overflow_check() {
  var y : Y = accept_integer(500)

  accept_integer(17) // expected-error{{ambiguous use of 'accept_integer'}}
  accept_integer(1000000) // expected-error{{ambiguous use of 'accept_integer'}}
}

// Coercion chaining.
struct meters : IntegerLiteralConvertible { 
  var value : Int8
  
  init(_ value: Int8) {
    self.value = value
  }

  typealias IntegerLiteralType = Int8
  init(integerLiteral value: Int8) {
    self.value = value
  }
}

struct supermeters : IntegerLiteralConvertible { // expected-error{{type 'supermeters' does not conform to protocol 'IntegerLiteralConvertible'}}
  var value : meters
  
  typealias IntegerLiteralType = meters // expected-note{{possibly intended match 'IntegerLiteralType' does not conform to '_BuiltinIntegerLiteralConvertible'}}
  init(_integerLiteral value: meters) {
    self.value = value
  }
}

func chaining() {
  var length : meters = 17;
  // FIXME: missing truncation warning <rdar://problem/14070127>.
  var long_length : meters = 500;
  var really_long_length : supermeters = 10
}

func memberaccess() {
  Int32(5.value) // expected-warning{{unused}}
  // This diagnostic is actually better than it looks, because the inner type is Builtin.Int32, not actually Int32.
  let x : Int32 = 7.value // expected-error{{'Int32' is not convertible to 'Int32'}}
  _ = x
}
