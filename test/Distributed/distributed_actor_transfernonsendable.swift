// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -swift-version 6 -verify -verify-ignore-unknown -target %target-swift-5.7-abi-triple -I %t %s -emit-sil

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
      print(y) // expected-error {{sending 'y' risks causing data races}}
      // expected-note @-1 {{task-isolated 'y' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  init(system: FakeActorSystem, y2: NonSendableKlass) {
    x = y2
    actorSystem = system
    _ = { @MainActor in
      print(y2) // expected-error {{sending 'y2' risks causing data races}}
      // expected-note @-1 {{'self'-isolated 'y2' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }

  distributed func transferActorField() async {
    await transferToMain(x) // expected-error {{sending 'self.x' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'self.x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }

  distributed func transferActorIsolatedArg(_ x: NonSendableKlass) async {
    await transferToMain(x) // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'self'-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'self'-isolated uses}}
  }

  distributed func transferActorIsolatedArgIntoClosure(_ x: NonSendableKlass) async {
    _ = { @MainActor in
      // TODO: In 2nd part of message should say actor-isolated instead of later
      // nonisolated uses in the case of a closure.
      print(x) // expected-error {{sending 'x' risks causing data races}}
      // expected-note @-1 {{'self'-isolated 'x' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
    }
  }


  func doSomething() async { }

  // Make sure that we consider asLocalActor's result to be the same actor as
  // its actor parameter.
  func testAsLocalActorForwards() async {
    await withTaskGroup { group in
      group.addTask {
        await self.doSomething()
      }
    }
  }
}

/////////////////////////////////
// MARK: Associated Type Tests //
/////////////////////////////////

protocol AssociatedTypeTestProtocol {
  associatedtype A: DistributedActor
}

func associatedTypeTestBasic<T: AssociatedTypeTestProtocol>(_: T, _: isolated T.A) {
}

func associatedTypeTestBasic2<T: AssociatedTypeTestProtocol>(_: T, iso: isolated T.A, x: NonSendableKlass) async {
  await transferToMain(x) // expected-error {{sending 'x' risks causing data races}}
  // expected-note @-1 {{sending 'iso'-isolated 'x' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and 'iso'-isolated uses}}
}
