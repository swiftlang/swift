// RUN: %swift %s -parse -verify

func isString(inout s: String) {}

func test_UnicodeScalarDoesNotImplementArithmetic(us: UnicodeScalar, i: Int) {
  var a1 = "a" + "b" // OK
  isString(&a1)
  let a2 = "a" - "b" // expected-error {{type 'UInt8' does not conform to protocol 'UnicodeScalarLiteralConvertible'}}
  let a3 = "a" * "b" // expected-error {{type 'UInt8' does not conform to protocol 'UnicodeScalarLiteralConvertible'}}
  let a4 = "a" / "b" // expected-error {{type 'UInt8' does not conform to protocol 'UnicodeScalarLiteralConvertible'}}

  let b1 = us + us // expected-error {{cannot invoke '+' with an argument list of type '(UnicodeScalar, UnicodeScalar)'}}
  let b2 = us - us // expected-error {{cannot invoke '-' with an argument list of type '(UnicodeScalar, UnicodeScalar)'}}
  let b3 = us * us // expected-error {{cannot invoke '*' with an argument list of type '(UnicodeScalar, UnicodeScalar)'}}
  let b4 = us / us // expected-error {{cannot invoke '/' with an argument list of type '(UnicodeScalar, UnicodeScalar)'}}

  let c1 = us + i // expected-error {{cannot invoke '+' with an argument list of type '(UnicodeScalar, Int)'}}
  let c2 = us - i // expected-error {{cannot invoke '-' with an argument list of type '(UnicodeScalar, Int)'}}
  let c3 = us * i // expected-error {{cannot invoke '*' with an argument list of type '(UnicodeScalar, Int)'}}
  let c4 = us / i // expected-error {{cannot invoke '/' with an argument list of type '(UnicodeScalar, Int)'}}

  let d1 = i + us // expected-error {{cannot invoke '+' with an argument list of type '(Int, UnicodeScalar)'}}
  let d2 = i - us // expected-error {{cannot invoke '-' with an argument list of type '(Int, UnicodeScalar)'}}
  let d3 = i * us // expected-error {{cannot invoke '*' with an argument list of type '(Int, UnicodeScalar)'}}
  let d4 = i / us // expected-error {{cannot invoke '/' with an argument list of type '(Int, UnicodeScalar)'}}
}

