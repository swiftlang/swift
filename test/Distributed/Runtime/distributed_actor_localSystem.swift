// RUN: %empty-directory(%t)
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Distributed

distributed actor Worker {
  typealias ActorSystem = LocalTestingDistributedActorSystem

  distributed func hi() {
    print("hi!")
  }

  nonisolated var description: Swift.String {
    "Worker(\(id))"
  }
}

// ==== Execute ----------------------------------------------------------------
@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()

    let actor = Worker(system: system)
    try await actor.hi() // local calls should still just work
    // CHECK: hi!
  }
}
