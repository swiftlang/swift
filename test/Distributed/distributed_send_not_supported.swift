// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-6.2-abi-triple -disable-availability-checking -enable-experimental-feature SendCalls -I %t %s

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// TODO: 'send' of distributed calls is not supported yet

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  distributed func hello() {}

  func local() {}

  func sendToSelf() {
    // A non-distributed method on the actor's own, known to be local, 'self'
    // can be sent like on any actor
    send self.local()

    // expected-error@+1:5{{'send' of distributed instance method 'hello()' is not supported yet}}
    send self.hello()
  }
}

func sendDistributed(_ greeter: Greeter) async throws {
  // expected-error@+1:3{{'send' of distributed instance method 'hello()' is not supported yet}}
  send greeter.hello()
}
