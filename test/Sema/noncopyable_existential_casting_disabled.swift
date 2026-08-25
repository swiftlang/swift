// RUN: %target-typecheck-verify-swift

// Regression test: without NoncopyableCasting enabled, casting from a
// noncopyable existential remains rejected exactly as before.

protocol P: ~Copyable {}

struct S: ~Copyable, P {
  var tag: Int
}

func takeExistential(_ box: consuming any P & ~Copyable) {
  if let s = box as? S { // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'S' always fails}}
    // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
    _ = s
  }
}

func forceCastExistential(_ box: consuming any P & ~Copyable) {
  let s = box as! S // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'S' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
  _ = s
}

func isExistential(_ box: consuming any P & ~Copyable) {
  _ = box is S // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'S' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
}
