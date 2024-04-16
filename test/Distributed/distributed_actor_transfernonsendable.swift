// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -swift-version 6 -verify -verify-ignore-unknown -disable-availability-checking -I %t %s -emit-sil

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

////////////////////////
// MARK: Declarations //
////////////////////////

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

final class NonSendableKlass {}

extension NonSendableKlass : Codable {}

@MainActor func transferToMain<T>(_ t: T) async {}

/////////////////
// MARK: Tests //
/////////////////

distributed actor MyDistributedActor {
  let x: NonSendableKlass

  init(system: FakeActorSystem, y: NonSendableKlass) {
    x = NonSendableKlass()
    actorSystem = system
    _ = { @MainActor in
      // TODO: This should error saying 'y' is actor isolated.
      print(y) // expected-error {{transferring 'y' may cause a data race}}
      // expected-note @-1 {{task-isolated 'y' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  init(system: FakeActorSystem, y2: NonSendableKlass) {
    x = y2
    actorSystem = system
    _ = { @MainActor in
      print(y2) // expected-error {{transferring 'y2' may cause a data race}}
      // expected-note @-1 {{'self'-isolated 'y2' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  distributed func transferActorField() async {
    await transferToMain(x) // expected-error {{transferring 'self.x' may cause a data race}}
    // expected-note @-1 {{transferring 'self'-isolated 'self.x' to main actor-isolated callee could cause races between main actor-isolated and 'self'-isolated uses}}
  }

  distributed func transferActorIsolatedArg(_ x: NonSendableKlass) async {
    await transferToMain(x) // expected-error {{transferring 'x' may cause a data race}}
    // expected-note @-1 {{transferring actor-isolated 'x' to main actor-isolated callee could cause races between main actor-isolated and actor-isolated uses}}
  }

  distributed func transferActorIsolatedArgIntoClosure(_ x: NonSendableKlass) async {
    _ = { @MainActor in
      // TODO: In 2nd part of message should say actor-isolated instead of later
      // nonisolated uses in the case of a closure.
      print(x) // expected-error {{transferring 'x' may cause a data race}}
      // expected-note @-1 {{actor-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }
}
