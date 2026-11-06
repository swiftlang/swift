// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// 'try send' on a '@oneway' distributed func is a direct call of its
// synchronous, 'nonisolated' distributed thunk. The thunk's parameters are
// 'sending', so the region checker (SE-0414) diagnoses the arguments just
// like for a 'try await' of a distributed func. The local branch of the thunk
// and the Embedded receive dispatcher (whose arguments are freshly decoded)
// enqueue the call without any diagnostics
//
// A non-'@oneway' distributed func is not part of this test: in Swift 6 mode
// its Embedded dispatcher arm has region isolation errors of its own

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

final class NonSendable: EmbeddedSerializationRequirement {
  var x = 0
  init() {}
  var serializedByteCount: Int { 0 }
  func encode(into output: inout OutputSpan<UInt8>) {}
  static func decode(from input: inout Span<UInt8>) throws -> NonSendable {
    NonSendable()
  }
}

distributed actor D {
  var state = NonSendable()

  @oneway distributed func take(_ n: NonSendable) {}

  // A call on the isolated 'self' is local, it is enqueued on the actor with
  // a closure which keeps the actor's isolation, rather than going through
  // the 'nonisolated' thunk, so passing the actor's own state is a same-actor
  // use, just like in a direct call
  func sendOwnState() {
    send self.take(state)
    state.x += 1
  }

  func sendOwnStateToOther(_ other: D) throws {
    try send other.take(state) // expected-error{{sending 'self.state' risks causing data races}}
    // expected-note@-1{{'self'-isolated 'self.state' is passed as a 'sending' parameter; Uses in callee may race with later 'self'-isolated uses}}
  }
}

func useAfterSend(_ d: D) throws {
  let ns = NonSendable()
  try send d.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' used after being passed as a 'sending' parameter; Later uses could race}}
  print(ns.x) // expected-note{{access can happen concurrently}}
}

func sendParameter(_ d: D, _ ns: NonSendable) throws {
  try send d.take(ns) // expected-error{{sending 'ns' risks causing data races}}
  // expected-note@-1{{'ns' is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
}

func sendFresh(_ d: D) throws {
  try send d.take(NonSendable())
}

func sendDisconnected(_ d: D) throws {
  let ns = NonSendable()
  ns.x = 1
  try send d.take(ns)
}
