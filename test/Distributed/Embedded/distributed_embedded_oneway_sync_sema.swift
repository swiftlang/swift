// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// In Embedded Swift a '@oneway' func with a synchronous body is called through
// a synchronous thunk ('distributed') or enqueued directly (plain and global
// actors), so 'send' calls of it are fine from synchronous code. A
// distributed call still throws, so it is spelled 'try send'

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  @oneway distributed func greet(_ n: Int) {}

  func selfCall() {
    // A call on the isolated 'self' is local, it is enqueued on the actor
    // rather than going through the synchronous thunk, and does not throw
    send self.greet(1)
  }
}

actor Counter {
  @oneway func bump() {}
  func plainBump() {}
}

@globalActor actor Background {
  static let shared = Background()
}

@Background @oneway func note() {}
@Background func plainNote() {}

// ==== -----------------------------------------------------------------------
// MARK: Distributed

func distributedFromSync(_ g: Greeter) throws {
  try send g.greet(1)
}

func distributedFromAsync(_ g: Greeter) async throws {
  try send g.greet(1)
}

func distributedMissingTry(_ g: Greeter) throws {
  send g.greet(1) // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
}

func distributedRequiresSend(_ g: Greeter) async throws {
  // Without 'send' this is not a call of the synchronous thunk, but a call of
  // the actor-isolated function itself
  // expected-error@+1{{actor-isolated distributed instance method 'greet' cannot be called from outside of the actor}}
  try g.greet(1)
}

// ==== -----------------------------------------------------------------------
// MARK: Plain and global actors

func plainFromSync(_ c: Counter) {
  send c.bump()
}

func globalFromSync() {
  send note()
}

// A synchronous actor-isolated function which is not '@oneway' is enqueued
// the same way
func plainNotOnewayFromSync(_ c: Counter) {
  send c.plainBump()
  send plainNote()
}
