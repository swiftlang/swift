// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait

// In Embedded Swift a 'oneway' func with a synchronous body is called through
// a synchronous thunk ('distributed') or enqueued directly (plain and global
// actors), so 'nowait' calls of it are fine from synchronous code. A
// distributed call still throws, so it is spelled 'try nowait'

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func greet(_ n: Int) oneway {}

  func selfCall() throws {
    // A known-local call goes through the synchronous thunk too
    try nowait self.greet(1)
  }
}

actor Counter {
  func bump() oneway {}
}

@globalActor actor Background {
  static let shared = Background()
}

@Background func note() oneway {}

// ==== -----------------------------------------------------------------------
// MARK: Distributed

func distributedFromSync(_ g: Greeter) throws {
  try nowait g.greet(1)
}

func distributedFromAsync(_ g: Greeter) async throws {
  try nowait g.greet(1)
}

func distributedMissingTry(_ g: Greeter) throws {
  nowait g.greet(1) // expected-error{{call can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to disable error propagation?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
}

func distributedRequiresNowait(_ g: Greeter) async throws {
  // expected-error@+1{{call to 'oneway' distributed instance method 'greet' must use 'nowait'}}
  try g.greet(1)
}

// ==== -----------------------------------------------------------------------
// MARK: Plain and global actors

func plainFromSync(_ c: Counter) {
  nowait c.bump()
}

func globalFromSync() {
  nowait note()
}
