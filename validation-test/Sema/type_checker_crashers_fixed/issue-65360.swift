// RUN: %target-typecheck-verify-swift

var a: Any?

let _: () -> Void = {
  for case (is Int)? in [a] {}
  if case (is Int, is Int) = a {} // expected-error {{cannot convert value of type 'Any?' to specified type '(_, _)'}}
}

let _: () -> Void = {
  for case (0)? in [a] {}
  if case (0, 0) = a {}
  // expected-error@-1 {{cannot convert value of type 'Any?' to specified type '(_, _)}}
}

let _: () -> Void = {
  for case (0)? in [a] {}
  for case (0, 0) in [a] {}
  // expected-error@-1 {{conflicting arguments to generic parameter 'Elements' ('[Any]' vs. '[Any?]')}}
  // expected-error@-2 {{conflicting arguments to generic parameter 'Element' ('Any' vs. 'Any?')}}
  // expected-error@-3 {{conflicting arguments to generic parameter 'Self' ('[Any]' vs. '[Any?]')}}
}
