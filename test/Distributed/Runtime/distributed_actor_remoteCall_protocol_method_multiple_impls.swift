// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Distributed
import FakeDistributedActorSystems

protocol GreeterProtocol<ActorSystem>: DistributedActor
  where ActorSystem: DistributedActorSystem<any Codable>,
        ActorSystem.ActorID: Codable {

  distributed func greet() -> String
}

protocol _FakeStub: DistributedActor {}
extension GreeterProtocol where Self: _FakeStub, ActorSystem.ActorID: Codable {
  distributed func greet() -> String {
    fatalError("STUB_IMPL")
  }
}

// TODO: remove manual stubs code
distributed actor __GreeterProtocolStub<ActorSystem>: GreeterProtocol, _FakeStub
    where ActorSystem: DistributedActorSystem<any Codable>, ActorSystem.ActorID: Codable {
  typealias ActorSystem = FakeRoundtripActorSystem
}

// ==== ------------------------------------------------------------------------

distributed actor GreeterImplOne: GreeterProtocol, _FakeStub {
  typealias ActorSystem = FakeRoundtripActorSystem
  distributed func greet() -> String { "IMPL ONE" }
}

distributed actor GreeterImplTwo: GreeterProtocol, _FakeStub {
  typealias ActorSystem = FakeRoundtripActorSystem
  distributed func greet() -> String { "IMPL TWO" }
}

// ==== ------------------------------------------------------------------------

@main struct Main {
  static func main() async throws {
    let fakeRoundtripSystem = FakeRoundtripActorSystem()

    let one = GreeterImplOne(actorSystem: fakeRoundtripSystem)
    let two = GreeterImplTwo(actorSystem: fakeRoundtripSystem)

    let targetingOne: any GreeterProtocol<FakeRoundtripActorSystem> = try __GreeterProtocolStub<FakeRoundtripActorSystem>.resolve(id: one.id, using: fakeRoundtripSystem)
    print("resolved on \(fakeRoundtripSystem): \(type(of: targetingOne))")
    // CHECK: resolved on main.FakeRoundtripActorSystem: __GreeterProtocolStub
//    let targetingOneReply = try await targetingOne.greet()
//    print("one reply: \(targetingOneReply)")

    let targetingTwo: any GreeterProtocol<FakeRoundtripActorSystem> = try __GreeterProtocolStub<FakeRoundtripActorSystem>.resolve(id: two.id, using: fakeRoundtripSystem)
    print("resolved on \(fakeRoundtripSystem): \(type(of: targetingTwo))")
    // CHECK: resolved on main.FakeRoundtripActorSystem: __GreeterProtocolStub
//    let targetingTwoReply = try await targetingTwo.greet()
//    print("two reply: \(targetingTwoReply)")

    print("ok") // CHECK: ok
  }
}


