// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableCasting

// REQUIRES: swift_feature_NoncopyableCasting

protocol P: ~Copyable {}
protocol Q: ~Copyable {}

struct S: ~Copyable, P {
  var tag: Int
}

struct Unrelated: ~Copyable, P {}

func takeExistential(_ box: consuming any P & ~Copyable) {
  // Existential source, concrete destination: now permitted.
  if let s = box as? S {
    _ = s
  }
}

func forceCastExistential(_ box: consuming any P & ~Copyable) {
  let s = box as! S
  _ = s
}

func isExistential(_ box: consuming any P & ~Copyable) {
  _ = box is S
}

func castToExistential(_ box: consuming any P & ~Copyable) {
  // Destination is still an existential: not yet supported.
  _ = box as? any Q & ~Copyable // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'any Q & ~Copyable' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
}

func castToArchetype<T: P & ~Copyable>(_ box: consuming any P & ~Copyable, _: T.Type) {
  // Destination is still an archetype: not yet supported.
  _ = box as? T // expected-warning {{cast from 'any P & ~Copyable' to unrelated type 'T' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
}

func castFromConcrete(_ mo: consuming S) {
  // Source is noncopyable but not an existential: still unsupported.
  _ = mo as? Unrelated // expected-warning {{cast from 'S' to unrelated type 'Unrelated' always fails}}
  // expected-error@-1 {{noncopyable types cannot be conditionally cast}}
}
