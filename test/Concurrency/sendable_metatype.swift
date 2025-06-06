// RUN: %target-typecheck-verify-swift -swift-version 6 -emit-sil -o /dev/null

// REQUIRES: concurrency

protocol P {
}

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
  Task.detached { // expected-warning{{risks causing data races}}
    x.g() // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

nonisolated func passMetaSmuggled<T: Q>(_: T.Type) {
  let x: Q.Type = T.self
  Task.detached { // expected-warning{{risks causing data races}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}

nonisolated func passMetaSmuggledAny<T: Q>(_: T.Type) {
  let x: Any.Type = T.self
  Task.detached {
    acceptMeta(x)
  }
}

nonisolated func captureThroughMetaValMoReqs<T>(_: T.Type) {
  let x = T.self
  Task.detached {
    _ = x
  }
}

nonisolated func passToMainActorSmuggledAny<T: Q>(_: T.Type) async {
  let x: Any.Type = T.self
  await acceptMetaOnMainActor(x)
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
nonisolated func passMetaSmuggledAnyFromExistential(_ pqT: (P & Q).Type) {
  let x: P.Type = pqT
  Task.detached { // expected-warning{{passing closure as a 'sending' parameter risks causing data races between code in the current task and concurrent execution of the closure}}
    acceptMeta(x) // expected-note{{closure captures 'x' which is accessible to code in the current task}}
  }
}


func testSendableMetatypeDowngrades() {
  @preconcurrency
  func acceptsSendableMetatype<T: SendableMetatype>(_: T.Type) {
  }

  func testWarning<T>(t: T.Type) { // expected-note {{consider making generic parameter 'T' conform to the 'SendableMetatype' protocol}} {{21-21=: SendableMetatype}}
    acceptsSendableMetatype(t) // expected-warning {{type 'T' does not conform to the 'SendableMetatype' protocol}}
  }

  func testWarning<T: P>(t: T.Type) { // expected-note {{consider making generic parameter 'T' conform to the 'SendableMetatype' protocol}} {{24-24= & SendableMetatype}}
    acceptsSendableMetatype(t) // expected-warning {{type 'T' does not conform to the 'SendableMetatype' protocol}}
  }
}
