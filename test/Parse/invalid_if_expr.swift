// RUN: %target-parse-verify-swift

func unbalanced_question(a: Bool, b: Bool, c: Bool, d: Bool) {
  (a ? b) // expected-error{{expected ':' after '? ...' in ternary}}
  (a ? b : c ? d) // expected-error{{expected ':' after '? ...' in ternary}}
  (a ? b ? c : d) // expected-error{{expected ':' after '? ...' in ternary}}
  (a ? b ? c) // expected-error{{expected ':' after '? ...' in ternary}} // expected-error{{expected ':' after '? ...' in ternary}}
}
