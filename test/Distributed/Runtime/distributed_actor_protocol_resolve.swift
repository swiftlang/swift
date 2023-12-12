// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

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
protocol GreeterP_ConcreteSystem: DistributedActor {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func greet() -> String
}

// start of @Proxy output =======
extension GreeterP_ConcreteSystem where Self == GreeterP_ConcreteSystem_Stub {
  static func resolve(
    id: ID, using system: ActorSystem
  ) throws -> any GreeterP_ConcreteSystem {
    print("\(Self.self).\(#function) -> return \(GreeterP_ConcreteSystem_Stub.self)")

    return try GreeterP_ConcreteSystem_Stub(actorSystem: system)
  }
}

distributed actor GreeterP_ConcreteSystem_Stub: GreeterP_ConcreteSystem {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func greet() -> String {
    let message = "STUB:\(Self.self).\(#function)"
    print(message)
    return message
  }
}
// end of @Proxy output =======

// ==== ------------------------------------------------------------------------


@main struct Main {
  static func main() async throws {
    let roundtripSystem = FakeRoundtripActorSystem()
    let localTestingSystem = LocalTestingDistributedActorSystem()

    // normal concrete type resolve calls:
    // let g0: Greeter = try Greeter.resolve(id: .init(parse: "one"), using: s)
    // let g1: Greeter = try .resolve(id: .init(parse: "one"), using: s)

    // Checking if errors are helpful:

    // what newcomer would expect to write:
    // let g3 = try GreeterP.resolve(id: .init(parse: "one"), using: s)
    // ERROR: Static member 'resolve' cannot be used on protocol metatype '(any GreeterP).Type'
    // TODO: maybe we could offer a better diagnostic specific to DA

    // let g2: GreeterP = try .resolve(id: .init(parse: "one"), using: s)
    // ERROR: Use of protocol 'GreeterP' as a type must be written 'any GreeterP'

    // YAY!
    let gx: any GreeterP_ConcreteSystem = try .resolve(id: .init(parse: "one"), using: roundtripSystem)
    // precondition(!__isLocalActor(gx), "Expected remote reference")
    _ = try await gx.greet()
    // CHECK: STUB:GreeterP_ConcreteSystem_Stub.greet()
  }
}


