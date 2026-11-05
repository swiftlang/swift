// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir
// RUN: %target-build-swift -module-name main  -target %target-swift-6.0-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import FakeDistributedActorSystems
import Distributed

@Resolvable
protocol EchoProtocol: DistributedActor, Codable where ActorSystem == FakeRoundtripActorSystem {
  distributed func echo(_ val: Int) -> Int
  distributed var number: Int { get }
}

distributed actor EchoActor: EchoProtocol {
  typealias ActorSystem = FakeRoundtripActorSystem

  distributed func echo(_ val: Int) -> Int {
    print("\(#function): \(val)")
    return val
  }

  distributed var number: Int {
    13
  }
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()

    let echo = EchoActor(actorSystem: system)
    let echoStub: any EchoProtocol = try $EchoProtocol.resolve(id: echo.id, using: system)

    let echoed = try await echoStub.echo(2)
    print("Echoed number = \(echoed)") // CHECK: Echoed number = 2

    // This used to fail because IRGen wrongly would miss to emit an accessor for the distributed property
    let number = try await echoStub.number
    print("Number = \(number)") // CHECK: Number = 13
  }
}