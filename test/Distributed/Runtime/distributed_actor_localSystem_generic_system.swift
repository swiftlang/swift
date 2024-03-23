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

import Distributed

@available(SwiftStdlib 6.0, *)
distributed actor Worker<ActorSystem> where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func distributedMethod() -> String {
    "implemented method"
  }

  distributed var distributedVariable: String {
    "implemented variable"
  }

  distributed func genericMethod<E: Codable>(_ value: E) async -> E {
    return value
  }
}

// ==== Execute ----------------------------------------------------------------
@available(SwiftStdlib 6.0, *)
@main struct Main {
  static func main() async throws {
    let system = LocalTestingDistributedActorSystem()

    let actor = Worker(actorSystem: system)

    let m = try await actor.distributedMethod()
    print("m = \(m)") // CHECK: m = implemented method

    let v = try await actor.distributedVariable
    print("v = \(v)") // CHECK: v = implemented variable

    let e = try await actor.genericMethod("echo")
    print("e = \(e)") // CHECK: e = echo


  }
}
