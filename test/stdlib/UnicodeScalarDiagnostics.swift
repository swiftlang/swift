// RUN: %target-typecheck-verify-swift

// FIXME: The clarity of these diagnostics could be improved.
// <rdar://problem/29912193>

func isString(_ s: inout String) {}

func test_UnicodeScalarDoesNotImplementArithmetic(_ us: UnicodeScalar, i: Int) {
  var a1 = "a" + "b" // OK
  isString(&a1)
  let a2 = "a" - "b" // expected-error {{binary operator '-' cannot be applied to two 'String' operands}}
  // expected-note@-1 {{overloads for '-' exist with these partially matching parameter lists: (Double, Double), (Float, Float), (Float16, Float16), (Float80, Float80), (Int, Int), (Int16, Int16), (Int32, Int32), (Int64, Int64), (Int8, Int8), (UInt, UInt), (UInt16, UInt16), (UInt32, UInt32), (UInt64, UInt64), (UInt8, UInt8)}}
  let a3 = "a" * "b" // expected-error {{binary operator '*' cannot be applied to two 'String' operands}}
  // expected-note@-1 {{overloads for '*' exist with these partially matching parameter lists: (Double, Double), (Float, Float), (Float16, Float16), (Float80, Float80), (Int, Int), (Int16, Int16), (Int32, Int32), (Int64, Int64), (Int8, Int8), (UInt, UInt), (UInt16, UInt16), (UInt32, UInt32), (UInt64, UInt64), (UInt8, UInt8)}}
  let a4 = "a" / "b" // expected-error {{binary operator '/' cannot be applied to two 'String' operands}}
  // expected-note@-1 {{overloads for '/' exist with these partially matching parameter lists: (Double, Double), (Float, Float), (Float16, Float16), (Float80, Float80), (Int, Int), (Int16, Int16), (Int32, Int32), (Int64, Int64), (Int8, Int8), (UInt, UInt), (UInt16, UInt16), (UInt32, UInt32), (UInt64, UInt64), (UInt8, UInt8)}}

  let b1 = us + us // expected-error {{binary operator '+' cannot be applied to two 'UnicodeScalar' (aka 'Unicode.Scalar') operands}}
  let b2 = us - us // expected-error {{binary operator '-' cannot be applied to two 'UnicodeScalar' (aka 'Unicode.Scalar') operands}}
  let b3 = us * us // expected-error {{binary operator '*' cannot be applied to two 'UnicodeScalar' (aka 'Unicode.Scalar') operands}}
  let b4 = us / us // expected-error {{binary operator '/' cannot be applied to two 'UnicodeScalar' (aka 'Unicode.Scalar') operands}}

  let c1 = us + i // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
  let c2 = us - i // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
  let c3 = us * i // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
  let c4 = us / i // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}

  let d1 = i + us // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
  let d2 = i - us // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
  let d3 = i * us // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
  let d4 = i / us // expected-error {{cannot convert value of type 'UnicodeScalar' (aka 'Unicode.Scalar') to expected argument type 'Int'}}
}

