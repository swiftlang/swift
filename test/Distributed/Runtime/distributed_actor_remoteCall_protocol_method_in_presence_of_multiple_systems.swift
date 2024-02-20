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

// @DistributedRemotelyJustViaProxyAccessible
protocol GreeterP_UnknownSystem: DistributedActor {
  distributed func greet() -> String
}

// TODO: remove manual stubs code
extension GreeterP_UnknownSystem where Self == GreeterP_UnknownSystem_FakeRoundtripActorSystem_Stub {
  static func resolve(
    id: ID, using system: ActorSystem
  ) throws -> any GreeterP_UnknownSystem {
    print("\(Self.self).\(#function) -> return \(GreeterP_UnknownSystem_FakeRoundtripActorSystem_Stub.self)")

    return try GreeterP_UnknownSystem_FakeRoundtripActorSystem_Stub(actorSystem: system)
  }
}

// TODO: remove manual stubs code
distributed actor GreeterP_UnknownSystem_FakeRoundtripActorSystem_Stub: GreeterP_UnknownSystem {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func greet() -> String {
    let message = "STUB:\(Self.self).\(#function)"
    print(message)
    return message
  }
}

// TODO: remove manual stubs code
extension GreeterP_UnknownSystem where Self == GreeterP_UnknownSystem_LocalTestingDistributedActorSystem_Stub {
  static func resolve(
    id: Self.ID, using system: Self.ActorSystem
  ) throws -> any GreeterP_UnknownSystem {
    print("\(Self.self).\(#function) -> return \(GreeterP_UnknownSystem_LocalTestingDistributedActorSystem_Stub.self)")

    return try GreeterP_UnknownSystem_LocalTestingDistributedActorSystem_Stub(actorSystem: system)
  }
}

// TODO: remove manual stubs code
distributed actor GreeterP_UnknownSystem_LocalTestingDistributedActorSystem_Stub: GreeterP_UnknownSystem {
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

    let gf: any GreeterP_UnknownSystem = try .resolve(id: fid, using: fakeRoundtripSystem)
    print("resolved on \(fakeRoundtripSystem): \(type(of: gf))")
    // CHECK: resolved on main.FakeRoundtripActorSystem: GreeterP_UnknownSystem_FakeRoundtripActorSystem_Stub
    print()

    let gl: any GreeterP_UnknownSystem = try .resolve(id: gid, using: localTestingSystem)
    print("resolved on \(localTestingSystem): \(type(of: gl))")
    // CHECK: resolved on Distributed.LocalTestingDistributedActorSystem: GreeterP_UnknownSystem_LocalTestingDistributedActorSystem_Stub

    print("ok") // CHECK: ok
  }
}


