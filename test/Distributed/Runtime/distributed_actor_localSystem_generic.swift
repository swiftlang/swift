// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -j2 -parse-as-library -I %t %s -o %t/a.out
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

@available(SwiftStdlib 6.0, *)
distributed actor Worker<ActorSystem> where ActorSystem: DistributedActorSystem<any Codable>, ActorSystem.ActorID: Codable {
  distributed func hi(name: String) {
    print("Hi, \(name)!")
  }

  nonisolated var description: Swift.String {
    "Worker(\(id))"
  }
}

// ==== Execute ----------------------------------------------------------------
@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()

    let actor = Worker(actorSystem: system)
    try await actor.hi(name: "P") // local calls should still just work
    // CHECK: Hi, P!
  }
}
