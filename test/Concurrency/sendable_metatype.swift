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

// -------------------------------------------------------------------------
// Conformance isolation for sending parameters
// -------------------------------------------------------------------------

protocol R: SendableMetatype { }

@MainActor
struct S: @MainActor P, R { }

@MainActor
class SC: @MainActor P, R { }

func acceptSendingP(_ s: sending any P) { }
func acceptSendingAnyObjectP(_ s: sending AnyObject & P) { }

func acceptSendingR(_ s: sending any R) { }
func acceptSendingAnyObjectR(_ s: sending AnyObject & R) { }

@MainActor func passSendingExistential<T: P, U: AnyObject & P , V: R, W: AnyObject & R>(
  t: sending T, u: sending U, v: sending V, w: sending W
) {
  acceptSendingP(S()) // expected-warning{{sending value of non-Sendable type 'S' risks causing data races}}
  // expected-note@-1{{Passing main actor-isolated value of non-Sendable type 'S' as a 'sending' parameter to global function 'acceptSendingP' risks causing races}}
  // expected-note@-2{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingP(SC()) // expected-warning{{sending value of non-Sendable type 'SC'}}
  // expected-note@-1{{Passing main actor-isolated value of non-Sendable type 'SC' as a 'sending' parameter to global function 'acceptSendingP' risks causing races}}' risks causing data races}}
  // expected-note@-2{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingAnyObjectP(SC()) // expected-warning{{sending value of non-Sendable type 'SC' risks causing data races}}
  // expected-note@-1{{Passing main actor-isolated value of non-Sendable type 'SC' as a 'sending' parameter to global function 'acceptSendingAnyObjectP' risks causing races}}' risks causing data races}}
  // expected-note@-2{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingP(t) // expected-warning{{sending 't' risks causing data races}}
  // expected-note@-1{{task-isolated 't' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  // expected-note@-2{{isolated conformance to protocol 'P' can be introduced he}}
  acceptSendingAnyObjectP(u) // expected-warning{{sending value of non-Sendable type 'U' risks causing data races}}
  // expected-note@-1{{task-isolated value of non-Sendable type 'U'}}
  // expected-note@-2{{isolated conformance to protocol 'P' can be introduced here}}
  // All of these are okay, because there are no isolated conformances to R.
  acceptSendingR(S())
  acceptSendingR(SC())
  acceptSendingAnyObjectR(SC())
  acceptSendingR(v)
  acceptSendingAnyObjectR(w)
}

func dynamicCastingExistential(
  _ s1: sending Any,
  _ s2: sending Any
) {
  if let s1p = s1 as? any P { // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
    acceptSendingP(s1p) // expected-warning{{sending 's1p' risks causing data races}}
    // expected-note@-1{{task-isolated 's1p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  } else {
    print(s1)
  }

  if let s2p = s2 as? any AnyObject & P { // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
    acceptSendingAnyObjectP(s2p) // expected-warning{{sending 's2p' risks causing data races}}
    // expected-note@-1{{task-isolated 's2p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  } else {
    print(s2)
  }
}

// @concurrent functions cannot pick up any isolated conformances dynamically,
// because they aren't on an actor. Make sure we don't complain.
@concurrent func dynamicCastingExistentialConcurrent(
  _ s1: sending Any,
  _ s2: sending Any
) async {
  if let s1p = s1 as? any P {
    acceptSendingP(s1p)
  } else {
    print(s1)
  }

  if let s2p = s2 as? any AnyObject & P {
    acceptSendingAnyObjectP(s2p)
  } else {
    print(s2)
  }
}

func dynamicCastingGeneric(
  _ s1: sending some Any,
  _ s2: sending some Any
) {
  if let s1p = s1 as? any P { // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
    acceptSendingP(s1p) // expected-warning{{sending 's1p' risks causing data races}}
    // expected-note@-1{{task-isolated 's1p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  } else {
    print(s1)
  }

  if let s2p = s2 as? any AnyObject & P { // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
    acceptSendingAnyObjectP(s2p) // expected-warning{{sending 's2p' risks causing data races}}
    // expected-note@-1{{task-isolated 's2p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
  } else {
    print(s2)
  }
}

@MainActor func dynamicCastingExistentialGood(
  _ s1: sending Any,
  _ s2: sending Any
) {
  if let s1p = s1 as? any R {
    acceptSendingR(s1p)
  } else {
    print(s1)
  }

  if let s2p = s2 as? any AnyObject & R {
    acceptSendingAnyObjectR(s2p)
  } else {
    print(s2)
  }
}

@MainActor func dynamicCastingGenericGood(
  _ s1: sending some Any,
  _ s2: sending some Any
) {
  if let s1p = s1 as? any R {
    acceptSendingR(s1p)
  } else {
    print(s1)
  }

  if let s2p = s2 as? any AnyObject & R {
    acceptSendingAnyObjectR(s2p)
  } else {
    print(s2)
  }
}

func forceCastingExistential(
  _ s1: sending Any,
  _ s2: sending AnyObject
) {
  let s1p = s1 as! any P // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingP(s1p) // expected-warning{{sending 's1p' risks causing data races}}
  // expected-note@-1{{task-isolated 's1p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}

  let s2p = s2 as! any AnyObject & P // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingAnyObjectP(s2p) // expected-warning{{sending 's2p' risks causing data races}}
  // expected-note@-1{{task-isolated 's2p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
}

@MainActor func forceCastingExistentialGood(
  _ s1: sending Any,
  _ s2: sending AnyObject
) {
  let s1p = s1 as! any R
  acceptSendingR(s1p) // okay

  let s2p = s2 as! any AnyObject & R
  acceptSendingAnyObjectR(s2p) // okay
}

func forceCastingGeneric(
  _ s1: sending some Any,
  _ s2: sending some AnyObject
) {
  let s1p = s1 as! any P // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingP(s1p) // expected-warning{{sending 's1p' risks causing data races}}
  // expected-note@-1{{task-isolated 's1p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}

  let s2p = s2 as! any AnyObject & P // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingAnyObjectP(s2p) // expected-warning{{sending 's2p' risks causing data races}}
  // expected-note@-1{{task-isolated 's2p' is passed as a 'sending' parameter; Uses in callee may race with later task-isolated uses}}
}

@MainActor func forceCastingGenericMainActor(
  _ s1: sending some Any,
  _ s2: sending some AnyObject
) {
  let s1p = s1 as! any P // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingP(s1p) // expected-warning{{sending 's1p' risks causing data races}}
  // expected-note@-1{{main actor-isolated 's1p' is passed as a 'sending' parameter; Uses in callee may race with later main actor-isolated uses}}

  let s2p = s2 as! any AnyObject & P // expected-note{{isolated conformance to protocol 'P' can be introduced here}}
  acceptSendingAnyObjectP(s2p) // expected-warning{{sending 's2p' risks causing data races}}
  // expected-note@-1{{main actor-isolated 's2p' is passed as a 'sending' parameter; Uses in callee may race with later main actor-isolated uses}}
}

@MainActor func forceCastingGenericGood(
  _ s1: sending some Any,
  _ s2: sending some AnyObject
) {
  let s1p = s1 as! any R
  acceptSendingR(s1p) // okay

  let s2p = s2 as! any AnyObject & R
  acceptSendingAnyObjectR(s2p) // okay
}
