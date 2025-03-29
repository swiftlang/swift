// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// rdar://90373022
// UNSUPPORTED: OS=watchos

import Distributed

distributed actor Worker {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  distributed func hi() {
    print("hi!")
  }

  distributed var greeting: String {
    "hi!"
  }

  nonisolated var description: Swift.String {
    "Worker(\(id))"
  }
}

// ==== Execute ----------------------------------------------------------------
@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()

    let actor = Worker(actorSystem: system)
    try await actor.hi() // local calls should still just work
    // CHECK: hi!

    let g = try await actor.greeting // local calls should still just work
    print("g = \(g)") // CHECK: g = hi!
  }
}
