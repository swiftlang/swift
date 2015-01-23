// RUN: %target-parse-verify-swift

func test_removedOperators(a: Bool, b: Bool) {
  var t1 = ~a // expected-error {{'~' is unavailable: use the '!' operator instead}}
  var t2 = a & b // expected-error {{'&' is unavailable: use the '&&' operator instead}}
  var t3 = a | b // expected-error {{'|' is unavailable: use the '||' operator instead}}
  var t4 = a ^ b // expected-error {{'^' is unavailable: use the '!=' operator instead}}
  t4 &= a // expected-error {{'&=' is unavailable: use the '&&' operator instead}}
  t4 |= a // expected-error {{'|=' is unavailable: use the '||' operator instead}}
  t4 ^= a // expected-error {{'^=' is unavailable: use the '!=' operator instead}}
}

