// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift

// RUN: %target-swift-frontend -I %t -emit-sil -strict-concurrency=complete -target %target-swift-5.7-abi-triple -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: distributed


import Distributed
import FakeDistributedActorSystems


typealias DefaultDistributedActorSystem = FakeActorSystem

final class NonSendableKlass {}

extension NonSendableKlass : Codable {}

@MainActor func transferToMain<T>(_ t: T) async {}

// ==== ------------------------------------------------------------------------

distributed actor MyDistributedActor {
  let x = NonSendableKlass()

  distributed func transferActorIsolatedArgIntoClosure(
    _ notSendableParamToDistributedFunc: NonSendableKlass) async {}
}
