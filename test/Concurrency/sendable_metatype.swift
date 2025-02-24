// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-experimental-feature StrictSendableMetatypes -emit-sil -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictSendableMetatypes


protocol Q {
  static func g()
}

nonisolated func acceptMeta<T>(_: T.Type) { }

@MainActor
func acceptMetaOnMainActor<T>(_: T.Type) { }

// -------------------------------------------------------------------------
// Non-Sendable metatype instances that cross into other isolation domains.
// -------------------------------------------------------------------------
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

nonisolated func passToMainActorSmuggledAny<T: Q>(_: T.Type) async {
  let x: Any.Type = T.self
  await acceptMetaOnMainActor(x) // expected-error{{sending value of non-Sendable type '(Any).Type' risks causing data races}}
  // expected-note@-1{{sending task-isolated value of non-Sendable type '(Any).Type' to main actor-isolated global function}}
}

// -------------------------------------------------------------------------
// Sendable metatype instances that cross into other isolation domains.
// -------------------------------------------------------------------------
nonisolated func passMetaWithSendableSmuggled<T: Sendable & Q>(_: T.Type) {
  let x: any (Q & Sendable).Type = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

nonisolated func passMetaWithSendableSmuggled<T: SendableMetatype & Q>(_: T.Type) {
  let x: any Q.Type = T.self
  Task.detached {
    acceptMeta(x) // okay, because T: SendableMetatype implies T.Type: Sendable
    x.g() // okay, because T: SendableMetatype implies T.Type: Sendable
  }
}

nonisolated func passSendableToMainActorSmuggledAny<T: Sendable>(_: T.Type) async {
  let x: Sendable.Type = T.self
  await acceptMetaOnMainActor(x)
}

// -------------------------------------------------------------------------
// Existential opening
// -------------------------------------------------------------------------
nonisolated func passMetaSmuggledAnyFromExistential(_ qT: Q.Type) {
  let x: Any.Type = qT
  Task.detached { // expected-error{{risks causing data races}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}
