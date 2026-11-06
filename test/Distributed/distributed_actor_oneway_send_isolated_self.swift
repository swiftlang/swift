// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// A 'send' on a distributed actor which is isolated to the caller, and
// therefore known to be local, is a local send of the function itself, which
// does not throw. Only a 'send' which may be remote, of a '@oneway'
// function, calls its distributed thunk and needs a 'try'. See
// distributed_actor_oneway_send_isolated_self_silgen.swift for the lowering

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Worker {
  @oneway distributed func tell(_ n: Int) {}
  @oneway distributed func tellAsync(_ n: Int) async {}
  distributed func plain(_ n: Int) {}

  func isolatedSends() {
    send self.tell(1)
    send self.tellAsync(2)
    send self.plain(3)
    send tell(4)

    try send self.tell(5)
    // expected-warning@-1{{no calls to throwing functions occur within 'try' expression}}
    try send self.tellAsync(6)
    // expected-warning@-1{{no calls to throwing functions occur within 'try' expression}}
  }

  distributed func distributedIsolatedSends() async {
    send self.tell(1)
    send self.tellAsync(2)
    send self.plain(3)
  }

  // A nonisolated 'self' may be remote
  nonisolated func kappa() throws {
    try send self.tell(1)
    try send self.tellAsync(2)

    send self.tell(3)
    // expected-error@-1{{call can throw but is not marked with 'try'}}
    // expected-note@-2{{did you mean to use 'try'?}}
    // expected-note@-3{{did you mean to handle error as optional value?}}
    // expected-note@-4{{did you mean to disable error propagation?}}
  }

  // The isolation of a closure is not known when the 'send' is lowered, so
  // even a 'send' in a closure which is isolated to the actor conservatively
  // calls the distributed thunk, and needs a 'try'
  func inClosure() throws {
    let c = { try send self.tell(1) }
    try c()

    let d = { send self.tell(2) }
    // expected-error@-1{{call can throw, but it is not marked with 'try' and the error is not handled}}
    d()
  }
}

// An 'isolated' parameter is known to be local too
func isolatedParameter(d: isolated Worker) {
  send d.tell(1)
  send d.tellAsync(2)
  send d.plain(3)
}

// A non-isolated reference may be remote
func nonisolatedReference(d: Worker) throws {
  try send d.tell(1)

  send d.tell(2)
  // expected-error@-1{{call can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  // expected-note@-4{{did you mean to disable error propagation?}}
}
