// RUN: %swift -I %S/.. %s -verify

struct int8 { 
  value : Builtin.Int8
  
  plus func convertFromIntegerLiteral(val : Builtin.Int8) -> int8 {
    return int8(val)
  }
}

struct int16 { 
  value : Builtin.Int16
  
  plus func convertFromIntegerLiteral(val : Builtin.Int16) -> int16 {
    return int16(val)
  }
}

struct int32 { 
  value : Builtin.Int32
  
  plus func convertFromIntegerLiteral(val : Builtin.Int32) -> int32 {
    return int32(val)
  }
}

func [infix_left] + (lhs : int8, rhs : int8) -> int8 {
  return int8(Builtin.add_Int8(lhs.value, rhs.value));
}

func [infix_left] + (lhs : int16, rhs : int16) -> int16 {
  return int16(Builtin.add_Int16(lhs.value, rhs.value));
}

func [infix_left] + (lhs : int32, rhs : int32) -> int32 {
  return int32(Builtin.add_Int32(lhs.value, rhs.value));
}

typealias IntegerLiteralType : int32

// Simple coercion of literals.
func simple() {
  var x1 : int8 = 1
  var x2 : int16 = 1
}


// Coercion of literals through operators.
func operators(x1 : int8) {
  var x2 : int8 = 1 + 2
  var x3 : int8 = 1 + x1
  var x4 : int8 = x2 + 1
  var x5 : int8 = x1 + x2 + 1 + 4 + x3 + 5
}

// Check coercion failure due to overflow.
struct X { }
struct Y { }
func accept_integer(x : int8) -> X { } // expected-note{{found this candidate}} // expected-note{{found this candidate}}
func accept_integer(x : int16) -> Y { } // expected-note{{found this candidate}} // expected-note{{found this candidate}}

func accept_y(y : Y) { }

func overflow_check() {
  var y = accept_integer(500);
  accept_y(y);
  accept_integer(17); // expected-error{{no candidates found for call}}
  accept_integer(1000000); // expected-error{{no candidates found for call}}
}
