// RUN: %target-typecheck-verify-swift

func unbalanced_question(a: Bool, b: Bool, c: Bool, d: Bool) {
  (a ? b) // expected-cparser-error{{expected ':' after '? ...' in ternary}} expected-astgen-error{{expected ':' and expression after '? ...' in ternary expression}}
  (a ? b : c ? d) // expected-cparser-error{{expected ':' after '? ...' in ternary}} expected-astgen-error{{expected ':' and expression after '? ...' in ternary expression}}
  (a ? b ? c : d) // expected-cparser-error{{expected ':' after '? ...' in ternary}} expected-astgen-error{{expected ':' and expression after '? ...' in ternary expression}}
  (a ? b ? c) // expected-cparser-error{{expected ':' after '? ...' in ternary}} expected-astgen-error{{expected ':' and expression after '? ...' in ternary expression}} // expected-cparser-error{{expected ':' after '? ...' in ternary}} expected-astgen-error{{expected ':' and expression after '? ...' in ternary expression}}
}
