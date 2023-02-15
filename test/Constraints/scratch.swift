// RUN: %target-typecheck-verify-swift
protocol P {}

struct S: P {}

func takesOptionalP(_: (some P)?) {}

func passOptional(value: (any P)?) {
  takesOptionalP(value) // expected-error{{asdf}}
}
