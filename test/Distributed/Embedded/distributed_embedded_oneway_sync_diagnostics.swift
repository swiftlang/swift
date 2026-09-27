// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed
// REQUIRES: swift_feature_OnewayNowait

// The diagnostics around the synchronous Embedded lowering of 'nowait' calls
// of 'oneway' funcs match the ones without Embedded

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

actor Counter {
  var n = 0

  func bump() oneway {}
  func bumpAsync(_ x: Int) async oneway {}

  func inside() {
    // A 'oneway' call must use 'nowait', even from the actor's own isolation
    bump() // expected-error{{call to 'oneway' instance method 'bump()' must use 'nowait'}}
    nowait bump()
    // Implicit 'self' is fine in both lowerings
    nowait bumpAsync(n)
  }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background func note() oneway {}

@Background func insideBackground() {
  note() // expected-error{{call to 'oneway' global function 'note()' must use 'nowait'}}
  nowait note()
}

distributed actor Greeter {
  distributed func greet() oneway {}

  func inside() throws {
    greet() // expected-error{{call to 'oneway' distributed instance method 'greet()' must use 'nowait'}}
    try nowait greet()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Effect markers

func superfluousTry(_ c: Counter) {
  // A plain actor 'oneway' call does not throw
  try nowait c.bump() // expected-warning{{no calls to throwing functions occur within 'try' expression}}
}

func superfluousAwait(_ c: Counter) async {
  // 'nowait' never waits, so an 'await' in front of it has no effect
  await nowait c.bump() // expected-warning{{no 'async' operations occur within 'await' expression}}
  await nowait c.bumpAsync(1) // expected-warning{{no 'async' operations occur within 'await' expression}}
}

func superfluousAwaitDistributed(_ g: Greeter) async throws {
  try await nowait g.greet() // expected-warning{{no 'async' operations occur within 'await' expression}}
}

func optionalTry(_ g: Greeter) {
  try? nowait g.greet()
}

// ==== -----------------------------------------------------------------------
// MARK: Function values

func functionValue(_ c: Counter) {
  let fn = c.bump // expected-error{{actor-isolated instance method 'bump()' can not be partially applied}}
  _ = fn
}

func distributedFunctionValue(_ g: Greeter) {
  let fn = g.greet // expected-error{{actor-isolated distributed instance method 'greet()' can not be partially applied}}
  _ = fn
}
