// RUN: %target-typecheck-verify-swift

var a: Any?

let _: () -> Void = {
  for case (is Int)? in [a] {}
  if case (is Int, is Int) = a {} // expected-error {{cannot convert value of type 'Any?' to specified type '(_, _)'}}
}
