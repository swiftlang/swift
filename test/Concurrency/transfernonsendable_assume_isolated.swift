// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -disable-availability-checking -verify %s -o /dev/null -swift-version 6

// REQUIRES: concurrency
// REQUIRES: asserts

// Tests for sending diagnostics through Actor.assumeIsolated and
// MainActor.assumeIsolated.

class NonSendableKlass {
  var x = 0
}

func useValue<T>(_ t: T) {}

// =============================================================================
// MARK: Actor.assumeIsolated — SentNeverSendable
// =============================================================================

actor ProtectsNonSendable {
  var ns: NonSendableKlass = .init()

  nonisolated func testParameter(_ nsArg: NonSendableKlass) {
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      isolatedSelf.ns = nsArg // expected-error {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'nsArg' which remains accessible to code in the current task}}
    }
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — UseAfterSend
// =============================================================================

actor ProtectsNonSendable2 {
  var ns: NonSendableKlass = .init()

  nonisolated func testUseAfterSend() {
    let ns2 = NonSendableKlass()
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      isolatedSelf.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    print(ns2.x) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: MainActor.assumeIsolated (static) — SentNeverSendable
// =============================================================================

func testMainActorAssumeIsolated(_ nsArg: NonSendableKlass) {
  MainActor.assumeIsolated {
    print(nsArg) // expected-error {{sending 'nsArg' risks causing data races}}
    // expected-note @-1 {{task-isolated 'nsArg' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
}

// =============================================================================
// MARK: MainActor.assumeIsolated (static) — UseAfterSend
// =============================================================================

func testMainActorUseAfterSend() {
  let ns2 = NonSendableKlass()
  MainActor.assumeIsolated {
    print(ns2) // expected-error {{sending 'ns2' risks causing data races}}
    // expected-note @-1 {{'ns2' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }
  print(ns2.x) // expected-note {{access can happen concurrently}}
}

// =============================================================================
// MARK: Actor.assumeIsolated — Out-of-line closure, SentNeverSendable
// =============================================================================

actor ProtectsNonSendable3 {
  var ns: NonSendableKlass = .init()

  nonisolated func testOutOfLineClosure(_ nsArg: NonSendableKlass) {
    let closure: (isolated ProtectsNonSendable3) -> () = { isolatedSelf in
      isolatedSelf.ns = nsArg // expected-error {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'nsArg' which remains accessible to code in the current task}}
    }
    self.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — Out-of-line closure, UseAfterSend
// =============================================================================

actor ProtectsNonSendable4 {
  var ns: NonSendableKlass = .init()

  nonisolated func testOutOfLineClosureUseAfterSend() {
    let ns2 = NonSendableKlass()
    let closure: (isolated ProtectsNonSendable4) -> () = { isolatedSelf in
      isolatedSelf.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    self.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
    print(ns2.x) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — UseAfterSend, no use after (safe)
// =============================================================================

actor ProtectsNonSendable5 {
  var ns: NonSendableKlass = .init()

  nonisolated func testNoUseAfterSend() {
    let ns2 = NonSendableKlass()
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = ns2
    }
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — UseAfterSend, read-only capture
// =============================================================================

actor ProtectsNonSendable6 {
  var ns: NonSendableKlass = .init()

  nonisolated func testCaptureReadOnly() {
    let ns2 = NonSendableKlass()
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      print(ns2) // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    useValue(ns2) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — UseAfterSend, multiple non-sendable captures
// =============================================================================

actor ProtectsNonSendable7 {
  var ns: NonSendableKlass = .init()
  var ns2: NonSendableKlass = .init()

  nonisolated func testMultipleNonSendableCaptures() {
    let a = NonSendableKlass()
    let b = NonSendableKlass()
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      isolatedSelf.ns = a // expected-error {{sending 'a' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'a' which remains accessible to code in the current task}}
      isolatedSelf.ns2 = b
    }
    useValue(a) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — UseAfterSend, two inline calls
// =============================================================================

actor ProtectsNonSendable8 {
  var ns: NonSendableKlass = .init()

  nonisolated func testTwoAssumeIsolatedCalls() {
    let ns2 = NonSendableKlass()
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      isolatedSelf.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      // expected-note @-1 {{access can happen concurrently}}
      isolatedSelf.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    useValue(ns2) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Actor.assumeIsolated — UseAfterSend, out-of-line closure used twice
// =============================================================================

actor ProtectsNonSendable9 {
  var ns: NonSendableKlass = .init()

  nonisolated func testOutOfLineClosureUsedTwice() {
    let ns2 = NonSendableKlass()
    let closure: (isolated ProtectsNonSendable9) -> () = { isolatedSelf in
      isolatedSelf.ns = ns2 // expected-error 2{{sending 'ns2' risks causing data races}}
      // expected-note @-1 2{{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    self.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
    self.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
    // expected-note @-1 {{access can happen concurrently}}
    useValue(ns2) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Cross-actor assumeIsolated — SentNeverSendable
// =============================================================================

actor CrossActorTarget {
  var ns: NonSendableKlass = .init()
}

actor CrossActorSender {
  func testParameterCrossActor(_ other: CrossActorTarget, _ nsArg: NonSendableKlass) {
    other.assumeIsolated { isolatedOther in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'other'}}
      isolatedOther.ns = nsArg // expected-error {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{'other'-isolated closure captures 'nsArg' which remains accessible to 'self'-isolated code}}
    }
  }
}

// =============================================================================
// MARK: Cross-actor assumeIsolated — UseAfterSend
// =============================================================================

extension CrossActorSender {
  func testUseAfterSendCrossActor(_ other: CrossActorTarget) {
    let ns2 = NonSendableKlass()
    other.assumeIsolated { isolatedOther in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'other'}}
      isolatedOther.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'other'-isolated closure captures 'ns2' which remains accessible to 'self'-isolated code}}
    }
    useValue(ns2) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Cross-actor assumeIsolated — Out-of-line closure, UseAfterSend
// =============================================================================

extension CrossActorSender {
  func testOutOfLineCrossActor(_ other: CrossActorTarget) {
    let ns2 = NonSendableKlass()
    let closure: (isolated CrossActorTarget) -> () = { isolatedOther in
      isolatedOther.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'other'-isolated closure captures 'ns2' which remains accessible to 'self'-isolated code}}
    }
    other.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'other'}}
    useValue(ns2) // expected-note {{access can happen concurrently}}
  }
}

// =============================================================================
// MARK: Cross-actor assumeIsolated — No use after (safe)
// =============================================================================

extension CrossActorSender {
  func testNoUseAfterSendCrossActor(_ other: CrossActorTarget) {
    let ns2 = NonSendableKlass()
    other.assumeIsolated { isolatedOther in
      isolatedOther.ns = ns2
    }
  }
}

// =============================================================================
// MARK: @MainActor to actor assumeIsolated — SentNeverSendable
// =============================================================================

@MainActor func testMainActorToActorParameter(_ other: CrossActorTarget, _ nsArg: NonSendableKlass) {
  other.assumeIsolated { isolatedOther in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'other'}}
    isolatedOther.ns = nsArg // expected-error {{sending 'nsArg' risks causing data races}}
    // expected-note @-1 {{'other'-isolated closure captures 'nsArg' which remains accessible to main actor-isolated code}}
  }
}

// =============================================================================
// MARK: @MainActor to actor assumeIsolated — UseAfterSend
// =============================================================================

@MainActor func testMainActorToActorUseAfterSend(_ other: CrossActorTarget) {
  let ns2 = NonSendableKlass()
  other.assumeIsolated { isolatedOther in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'other'}}
    isolatedOther.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
    // expected-note @-1 {{'other'-isolated closure captures 'ns2' which remains accessible to main actor-isolated code}}
  }
  useValue(ns2) // expected-note {{access can happen concurrently}}
}
