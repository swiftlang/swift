// RUN: %target-typecheck-verify-swift -target %target-future-triple -enable-experimental-feature NoncopyableCasting

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
  // Destination is an existential: permitted, and the cast can succeed at
  // runtime, so it is not diagnosed as always failing.
  _ = box as? any Q & ~Copyable
}

func castToArchetype<T: P & ~Copyable>(_ box: consuming any P & ~Copyable, _: T.Type) {
  // Destination is an archetype: permitted.
  _ = box as? T
}

func castFromConcrete(_ mo: consuming S) {
  // A non-existential noncopyable source is permitted. These two types are
  // unrelated, so the cast is still diagnosed as always failing.
  _ = mo as? Unrelated // expected-warning {{cast from 'S' to unrelated type 'Unrelated' always fails}}
}

func castFromGeneric<T: ~Copyable>(_ value: consuming T) {
  _ = value as? S
}

func forceCastToExistential(_ box: consuming any P & ~Copyable) {
  _ = box as! any Q & ~Copyable
}
