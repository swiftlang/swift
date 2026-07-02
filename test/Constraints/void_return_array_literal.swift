// RUN: %target-typecheck-verify-swift

func voidReturnRandomElement() {
  return [1, 2, 3].randomElement()! // expected-error {{unexpected non-void return value in void function}}
  // expected-note@-1 {{did you mean to add a return type?}}
}

func voidReturnFirst() {
  return [1, 2, 3].first! // expected-error {{unexpected non-void return value in void function}}
  // expected-note@-1 {{did you mean to add a return type?}}
}

func voidReturnLast() {
  return [1, 2, 3].last! // expected-error {{unexpected non-void return value in void function}}
  // expected-note@-1 {{did you mean to add a return type?}}
}

// Typed variable should still work (was never broken).
func voidReturnTypedVariable() {
  let a: [Int] = [1, 2, 3]
  return a.randomElement()! // expected-error {{unexpected non-void return value in void function}}
  // expected-note@-1 {{did you mean to add a return type?}}
}
