// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -swift-version 6 -verify -verify-ignore-unknown -disable-availability-checking -I %t %s -emit-sil

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

class NonSendableKlass {
  var x = 0
}

// =============================================================================
// MARK: DistributedActor.assumeIsolated — SentNeverSendable
// =============================================================================

distributed actor MyDistributedActor {
  var ns: NonSendableKlass = .init()

  nonisolated func testParameter(_ nsArg: NonSendableKlass) {
    self.assumeIsolated { isolatedSelf in // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
      isolatedSelf.ns = nsArg // expected-error {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'nsArg' which remains accessible to code in the current task}}
    }
  }
}

// =============================================================================
// MARK: DistributedActor.assumeIsolated — UseAfterSend
// =============================================================================

distributed actor MyDistributedActor2 {
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
// MARK: DistributedActor.assumeIsolated — Out-of-line closure, SentNeverSendable
// =============================================================================

distributed actor MyDistributedActor3 {
  var ns: NonSendableKlass = .init()

  nonisolated func testOutOfLineClosure(_ nsArg: NonSendableKlass) {
    let closure: (isolated MyDistributedActor3) -> () = { isolatedSelf in
      isolatedSelf.ns = nsArg // expected-error {{sending 'nsArg' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'nsArg' which remains accessible to code in the current task}}
    }
    self.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
  }
}

// =============================================================================
// MARK: DistributedActor.assumeIsolated — Out-of-line closure, UseAfterSend
// =============================================================================

distributed actor MyDistributedActor4 {
  var ns: NonSendableKlass = .init()

  nonisolated func testOutOfLineClosureUseAfterSend() {
    let ns2 = NonSendableKlass()
    let closure: (isolated MyDistributedActor4) -> () = { isolatedSelf in
      isolatedSelf.ns = ns2 // expected-error {{sending 'ns2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated closure captures 'ns2' which remains accessible to code in the current task}}
    }
    self.assumeIsolated(closure) // expected-note {{passing closure to 'assumeIsolated' here causes closure to become isolated to 'self'}}
    print(ns2.x) // expected-note {{access can happen concurrently}}
  }
}
