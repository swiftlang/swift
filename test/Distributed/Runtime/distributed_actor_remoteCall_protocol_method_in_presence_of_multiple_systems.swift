// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-build-swift -module-name main  -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir -o %t/a.out
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

// FIXME(distributed): enable via @_DistributedProtocol
protocol GreeterProtocol: DistributedActor {
  distributed func greet() -> String
}

// TODO: remove manual stubs code
extension GreeterProtocol where Self == GreeterProtocol_FakeRoundtripActorSystem_Stub {
  static func resolve(
    id: ID, using system: ActorSystem
  ) throws -> any GreeterProtocol {
    print("\(Self.self).\(#function) -> return \(GreeterProtocol_FakeRoundtripActorSystem_Stub.self)")

    return try GreeterProtocol_FakeRoundtripActorSystem_Stub(actorSystem: system)
  }
}

// TODO: remove manual stubs code
distributed actor GreeterProtocol_FakeRoundtripActorSystem_Stub: GreeterProtocol {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func greet() -> String {
    let message = "STUB:\(Self.self).\(#function)"
    print(message)
    return message
  }
}

// TODO: remove manual stubs code
extension GreeterProtocol where Self == GreeterProtocol_LocalTestingDistributedActorSystem_Stub {
  static func resolve(
    id: Self.ID, using system: Self.ActorSystem
  ) throws -> any GreeterProtocol {
    print("\(Self.self).\(#function) -> return \(GreeterProtocol_LocalTestingDistributedActorSystem_Stub.self)")

    return try GreeterProtocol_LocalTestingDistributedActorSystem_Stub(actorSystem: system)
  }
}

// TODO: remove manual stubs code
distributed actor GreeterProtocol_LocalTestingDistributedActorSystem_Stub: GreeterProtocol {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  distributed func greet() -> String {
    let message = "STUB:\(Self.self).\(#function)"
    print(message)
    return message
  }
}

// ==== ------------------------------------------------------------------------

distributed actor DAF {
  typealias ActorSystem = FakeRoundtripActorSystem
}
distributed actor DAL {
  typealias ActorSystem = LocalTestingDistributedActorSystem
}


@main struct Main {
  static func main() async throws {
    let fakeRoundtripSystem = FakeRoundtripActorSystem()
    let fid = fakeRoundtripSystem.assignID(DAF.self)

    let localTestingSystem = LocalTestingDistributedActorSystem()
    let gid = localTestingSystem.assignID(DAL.self)

    let gf: any GreeterProtocol = try .resolve(id: fid, using: fakeRoundtripSystem)
    print("resolved on \(fakeRoundtripSystem): \(type(of: gf))")
    // CHECK: resolved on main.FakeRoundtripActorSystem: GreeterProtocol_FakeRoundtripActorSystem_Stub
    print()

    let gl: any GreeterProtocol = try .resolve(id: gid, using: localTestingSystem)
    print("resolved on \(localTestingSystem): \(type(of: gl))")
    // CHECK: resolved on Distributed.LocalTestingDistributedActorSystem: GreeterProtocol_LocalTestingDistributedActorSystem_Stub

    print("ok") // CHECK: ok
  }
}


