// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-build-swift -module-name ActorsFramework -target %target-swift-6.0-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_DUMP_ACCESSIBLE_FUNCTIONS=true %target-run %t/a.out 2>&1 | %FileCheck %s --color --dump-input=always

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

// ==== Known actor system -----------------------------------------------------

public struct Response: Codable, Sendable {
  let resp: Int
  public init(resp: Int) {
    self.resp = resp
  }
}

public struct Provider: Codable, Sendable {
  let r1: Int
  public init(r1: Int) {
    self.r1 = r1
  }
}

@Resolvable
public protocol DistributedNotificationService: DistributedActor
where ActorSystem == FakeRoundtripActorSystem {
  distributed func getArray(a1: [Int], a2: String?) async throws -> [Response]
}

distributed actor DistributedNotificationServiceImpl: DistributedNotificationService {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func getArray(a1: [Int], a2: String?) async throws -> [Response] {
    []  // mock impl is enough
  }
}

// ==== ------------------------------------------------------------------------

@main struct Main {
  static func main() async throws {
    let roundtripSystem = FakeRoundtripActorSystem()

    let real: any DistributedNotificationService = DistributedNotificationServiceImpl(
      actorSystem: roundtripSystem)

    let proxy: any DistributedNotificationService =
      try $DistributedNotificationService.resolve(id: real.id, using: roundtripSystem)
    _ = try await proxy.getArray(a1: [], a2: "")

    // CHECK: ==== Accessible Function Records ====

    // CHECK: Record name: $s15ActorsFramework30DistributedNotificationServicePAA0C001_C9ActorStubRzrlE8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
    // CHECK:  Demangled: distributed thunk (extension in ActorsFramework):ActorsFramework.DistributedNotificationService< where A: Distributed._DistributedActorStub>.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ActorsFramework.Response>
    // CHECK:  Function Ptr: [[PTR:0x.*]]
    // CHECK:  Flags.IsDistributed: 1

    // CHECK: Record name: $s15ActorsFramework34DistributedNotificationServiceImplC8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
    // CHECK:  Demangled: distributed thunk ActorsFramework.DistributedNotificationServiceImpl.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ActorsFramework.Response>
    // CHECK:  Function Ptr: [[PTR:0x.*]]
    // CHECK:  Flags.IsDistributed: 1

    // CHECK: Record name: $s15ActorsFramework31$DistributedNotificationServiceC8getArray2a12a2SayAA8ResponseVGSaySiG_SSSgtYaKFTE
    // CHECK:  Demangled: distributed thunk ActorsFramework.$DistributedNotificationService.getArray(a1: Swift.Array<Swift.Int>, a2: Swift.Optional<Swift.String>) async throws -> Swift.Array<ActorsFramework.Response>
    // CHECK:  Function Ptr: [[PTR:0x.*]]
    // CHECK:  Flags.IsDistributed: 1

    // CHECK: Record count: 3
    // CHECK: ==== End of Accessible Function Records ====
  }
}
