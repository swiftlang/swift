// RUN: %swift %s -verify

func foreach_variable() {
  for i in 0..42 {
    i = 11   // expected-error {{cannot assign to read-only variable or subscript}}
  }
}
