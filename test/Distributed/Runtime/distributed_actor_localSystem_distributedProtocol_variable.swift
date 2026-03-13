// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -j2 -parse-as-library -I %t %s -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// Run again with library evolution:
// RUN: %target-build-swift -module-name main -j2 -parse-as-library -enable-library-evolution -I %t %s -plugin-path %swift-plugin-dir -o %t/evo.out
// RUN: %target-codesign %t/evo.out
// RUN: %target-run %t/evo.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// rdar://90373022
// UNSUPPORTED: OS=watchos

import Distributed

@Resolvable
@available(SwiftStdlib 6.0, *)
protocol WorkerProtocol: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  distributed var distributedVariable: String { get }
}

@available(SwiftStdlib 6.0, *)
distributed actor Worker: WorkerProtocol {
  distributed var distributedVariable: String {
    "implemented variable"
  }
}

// ==== Execute ----------------------------------------------------------------


@available(SwiftStdlib 6.0, *)
func test_distributedVariable<DA: WorkerProtocol>(actor: DA) async throws -> String {
  try await actor.distributedVariable
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()

    let actor: any WorkerProtocol = Worker(actorSystem: system)

    // force a call through witness table
    let v1 = try await test_distributedVariable(actor: actor)
    print("v1 = \(v1)") // CHECK: v1 = implemented variable
  }
}
