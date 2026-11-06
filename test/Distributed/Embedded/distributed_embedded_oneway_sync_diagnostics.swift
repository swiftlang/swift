// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// The diagnostics around the synchronous Embedded lowering of 'send' calls
// of '@oneway' funcs match the ones without Embedded

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

actor Counter {
  var n = 0

  @oneway func bump() {}
  @oneway func bumpAsync(_ x: Int) async {}

  func inside() {
    // A '@oneway' call from the actor's own isolation is a plain call
    bump()
    send bump()
    // Implicit 'self' is fine in both lowerings
    send bumpAsync(n)
  }
}

@globalActor actor Background {
  static let shared = Background()
}

@Background @oneway func note() {}

@Background func insideBackground() {
  note()
  send note()
}

distributed actor Greeter {
  @oneway distributed func greet() {}

  func inside() throws {
    greet()
    send greet()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Effect markers

func superfluousTry(_ c: Counter) {
  // A plain actor '@oneway' call does not throw
  try send c.bump() // expected-warning{{no calls to throwing functions occur within 'try' expression}}
}

func awaitSend(_ c: Counter) async {
  // 'send' never waits, so it cannot be combined with 'await'
  await send c.bump() // expected-error@:3{{'await' cannot be combined with 'send'; an operation cannot be both awaited on and sent without waiting for its result}}{{3-9=}}
  await send c.bumpAsync(1) // expected-error@:3{{'await' cannot be combined with 'send'}}{{3-9=}}
  send await c.bump() // expected-error@:8{{'send' cannot be combined with 'await'}}{{8-14=}}
}

func awaitSendDistributed(_ g: Greeter) async throws {
  try await send g.greet() // expected-error@:7{{'await' cannot be combined with 'send'}}{{7-13=}}
}

func optionalTry(_ g: Greeter) {
  try? send g.greet()
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
