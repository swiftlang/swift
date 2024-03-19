// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -j2 -parse-as-library -I %t %s -plugin-path %swift-plugin-dir -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// rdar://90373022
// UNSUPPORTED: OS=watchos

import Distributed

// @_DistributedProtocol
@available(SwiftStdlib 6.0, *)
protocol WorkerProtocol: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  //  distributed func hi(name: String)
  distributed var distributedVariable: String { get }
}

@available(SwiftStdlib 6.0, *)
distributed actor Worker: WorkerProtocol {
  distributed var distributedVariable: String {
    "implemented!"
  }

//  distributed func generic<T: Codable & Sendable>(incoming: T) throws -> T {
//    return incoming
//  }
}

@available(SwiftStdlib 6.0, *)
extension WorkerProtocol {
    distributed var distributedVariable: String { "" }
}

// ==== Execute ----------------------------------------------------------------


@available(SwiftStdlib 6.0, *)
func test<DA: WorkerProtocol>(actor: DA) async throws {
    _ = try await actor.distributedVariable
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()

    let actor = Worker(actorSystem: system)
    try await test(actor: actor)

//    // local calls should still just work
//
//    let v = try await actor.distributedVariable
//    print("v = \(v)") // CHECK: v = implemented!


  }
}
