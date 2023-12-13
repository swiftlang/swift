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

// ==== Known actor system -----------------------------------------------------

// @DistributedRemotelyJustViaProxyAccessible
protocol GreeterP_ConcreteSystem: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  distributed func greet() -> String
}

// start of @Proxy output =======
extension GreeterP_ConcreteSystem where Self == GreeterP_ConcreteSystem_Stub {
  static func resolve(
    id: ID, using system: ActorSystem
  ) throws -> any GreeterP_ConcreteSystem {
    print("\(Self.self).\(#function) -> return \(GreeterP_ConcreteSystem_Stub.self)")

    return try GreeterP_ConcreteSystem_Stub.resolve(id: id, using: system)
  }
}

distributed actor GreeterP_ConcreteSystem_Stub: GreeterP_ConcreteSystem {
  typealias ActorSystem = FakeRoundtripActorSystem
  let hint: String
  init(actorSystem: ActorSystem) {
    self.hint = ""
    self.actorSystem = actorSystem
  }
  init(hint: String, actorSystem: ActorSystem) {
    self.hint = hint
    self.actorSystem = actorSystem
  }

  distributed func greet() -> String {
    let message = "STUB[\(self.hint)]:\(Self.self).\(#function)"
    print(message)
    return message
  }

  func call_greet<Act: GreeterP_ConcreteSystem>(actor: Act/*, ... params ... */) async throws -> String {
    try await actor.greet()
  }
}
// end of @Proxy output =======

distributed actor GreeterImpl: GreeterP_ConcreteSystem {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func greet() -> String {
    "[IMPL]:Hello from \(Self.self)"
  }
}

// ==== ------------------------------------------------------------------------


@main struct Main {
  static func main() async throws {
    let roundtripSystem = FakeRoundtripActorSystem()
    let localTestingSystem = LocalTestingDistributedActorSystem()

    let real: any GreeterP_ConcreteSystem = GreeterImpl(actorSystem: roundtripSystem)
    let realGreeting = try await real.greet()
    print("local call greeting: \(realGreeting)")
    // CHECK: local call greeting: [IMPL]:Hello from GreeterImpl

    let proxy: any GreeterP_ConcreteSystem = try .resolve(id: real.id, using: roundtripSystem)
    let greeting = try await proxy.greet()
    // CHECK: >> remoteCall: on:main.GreeterP_ConcreteSystem_Stub, target:greet(), invocation:FakeInvocationEncoder(genericSubs: [], arguments: [], returnType: Optional(Swift.String), errorType: nil), throwing:Swift.Never, returning:Swift.String
    // CHECK: > execute distributed target: greet(), identifier: $s4main23GreeterP_ConcreteSystemP5greetSSyFTE

    // CHECK: << remoteCall return: [IMPL]:Hello from GreeterImpl

    print("protocol call greeting: \(greeting)")
    // CHECK: protocol call greeting: [IMPL]:Hello from GreeterImpl
  }
}


