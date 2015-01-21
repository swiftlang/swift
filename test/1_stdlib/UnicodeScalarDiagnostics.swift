// RUN: %target-parse-verify-swift

func isString(inout s: String) {}

func test_UnicodeScalarDoesNotImplementArithmetic(us: UnicodeScalar, i: Int) {
  var a1 = "a" + "b" // OK
  isString(&a1)
  let a2 = "a" - "b" // expected-error {{binary operator '-' cannot be applied to two String operands}}
  let a3 = "a" * "b" // expected-error {{binary operator '*' cannot be applied to two String operands}}
  let a4 = "a" / "b" // expected-error {{binary operator '/' cannot be applied to two String operands}}

  let b1 = us + us // expected-error {{binary operator '+' cannot be applied to two UnicodeScalar operands}}
  let b2 = us - us // expected-error {{binary operator '-' cannot be applied to two UnicodeScalar operands}}
  let b3 = us * us // expected-error {{binary operator '*' cannot be applied to two UnicodeScalar operands}}
  let b4 = us / us // expected-error {{binary operator '/' cannot be applied to two UnicodeScalar operands}}

  let c1 = us + i // expected-error {{binary operator '+' cannot be applied to operands of type 'UnicodeScalar' and 'Int'}} expected-note{{Overloads for '+' exist with these partially matching parameter lists:}}
  let c2 = us - i // expected-error {{binary operator '-' cannot be applied to operands of type 'UnicodeScalar' and 'Int'}} expected-note{{Overloads for '-' exist with these partially matching parameter lists:}}
  let c3 = us * i // expected-error {{binary operator '*' cannot be applied to operands of type 'UnicodeScalar' and 'Int'}} expected-note{{Overloads for '*' exist with these partially matching parameter lists:}}
  let c4 = us / i // expected-error {{binary operator '/' cannot be applied to operands of type 'UnicodeScalar' and 'Int'}} expected-note{{Overloads for '/' exist with these partially matching parameter lists:}}

  let d1 = i + us // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'UnicodeScalar'}} expected-note{{Overloads for '+' exist with these partially matching parameter lists:}}
  let d2 = i - us // expected-error {{binary operator '-' cannot be applied to operands of type 'Int' and 'UnicodeScalar'}} expected-note{{Overloads for '-' exist with these partially matching parameter lists:}}
  let d3 = i * us // expected-error {{binary operator '*' cannot be applied to operands of type 'Int' and 'UnicodeScalar'}} expected-note{{Overloads for '*' exist with these partially matching parameter lists:}}
  let d4 = i / us // expected-error {{binary operator '/' cannot be applied to operands of type 'Int' and 'UnicodeScalar'}} expected-note{{Overloads for '/' exist with these partially matching parameter lists:}}
}

