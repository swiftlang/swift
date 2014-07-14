// RUN: %swift %s -parse -verify

func isString(inout s: String) {}

func test_UnicodeScalarDoesNotImplementArithmetic(us: UnicodeScalar, i: Int) {
  var a1 = "a" + "b" // OK
  isString(&a1)
  let a2 = "a" - "b" // expected-error {{type 'UInt8' does not conform to protocol 'ExtendedGraphemeClusterLiteralConvertible'}}
  let a3 = "a" * "b" // expected-error {{type 'UInt8' does not conform to protocol 'ExtendedGraphemeClusterLiteralConvertible'}}
  let a4 = "a" / "b" // expected-error {{type 'UInt8' does not conform to protocol 'ExtendedGraphemeClusterLiteralConvertible'}}

  let b1 = us + us // expected-error {{'UnicodeScalar' is not convertible to 'UInt8'}}
  let b2 = us - us // expected-error {{'UnicodeScalar' is not convertible to 'UInt8'}}
  let b3 = us * us // expected-error {{'UnicodeScalar' is not convertible to 'UInt8'}}
  let b4 = us / us // expected-error {{'UnicodeScalar' is not convertible to 'UInt8'}}

  let c1 = us + i // expected-error {{'UnicodeScalar' is not convertible to 'Int'}}
  let c2 = us - i // expected-error {{'UnicodeScalar' is not convertible to 'Int'}}
  let c3 = us * i // expected-error {{'UnicodeScalar' is not convertible to 'Int'}}
  let c4 = us / i // expected-error {{'UnicodeScalar' is not convertible to 'Int'}}

  let d1 = i + us // expected-error {{'Int' is not convertible to 'UInt8'}}
  let d2 = i - us // expected-error {{'Int' is not convertible to 'UInt8'}}
  let d3 = i * us // expected-error {{'Int' is not convertible to 'UInt8'}}
  let d4 = i / us // expected-error {{'Int' is not convertible to 'UInt8'}}
}

