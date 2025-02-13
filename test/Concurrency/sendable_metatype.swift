// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-experimental-feature StrictSendableMetatypes -emit-sil -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictSendableMetatypes


protocol Q {
  static func g()
}

nonisolated func acceptMeta<T>(_: T.Type) { }

// -------------------------------------------------------------------------
// Non-Sendable metatype instances that cross into other isolation domains.
// -------------------------------------------------------------------------
nonisolated func staticCallThroughMeta<T: Q>(_: T.Type) {
  let x = T.self
  Task.detached { // expected-error{{risks causing data races}}
    x.g() // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

nonisolated func passMeta<T: Q>(_: T.Type) {
  let x = T.self
  Task.detached { // expected-error{{risks causing data races}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

nonisolated func staticCallThroughMetaSmuggled<T: Q>(_: T.Type) {
  let x: Q.Type = T.self
  Task.detached { // expected-error{{risks causing data races}}
    x.g() // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

nonisolated func passMetaSmuggled<T: Q>(_: T.Type) {
  let x: Q.Type = T.self
  Task.detached { // expected-error{{risks causing data races}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

nonisolated func passMetaSmuggledAny<T: Q>(_: T.Type) {
  let x: Any.Type = T.self
  Task.detached { // expected-error{{risks causing data races}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

// -------------------------------------------------------------------------
// Sendable metatype instances that cross into other isolation domains.
// -------------------------------------------------------------------------
nonisolated func passMetaWithSendable<T: Sendable & Q>(_: T.Type) {
  let x = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

nonisolated func passMetaWithSendableSmuggled<T: Sendable & Q>(_: T.Type) {
  let x: any (Q & Sendable).Type = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

// -------------------------------------------------------------------------
// Sendable requirements on metatypes
// -------------------------------------------------------------------------

nonisolated func passMetaWithMetaSendable<T: Q>(_: T.Type) where T.Type: Sendable {
  // FIXME: Bogus errors below because we don't currently handle constraints on
  // metatypes.
  let x = T.self
  Task.detached { // expected-error{{risks causing data races}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
                  // should be okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

// TODO: Cannot currently express an existential or opaque type where the
// metatype is Sendable.
