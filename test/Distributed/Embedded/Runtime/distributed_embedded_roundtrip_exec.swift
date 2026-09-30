// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -plugin-path %swift-plugin-dir %s %S/Inputs/ResolvableWorker.swift %S/Inputs/PortableRoundtripActorSystem.swift -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// End-to-end distributed round-trips in Embedded Swift.

import _Concurrency
import Distributed

distributed actor Greeter {
  distributed func hello(name: String) -> String {
    return "Hello, \(name)!"
  }
}

distributed actor Hub {
  distributed func dispatch(to worker: any ResolvableWorker) async throws -> String {
    return try await worker.work(name: "world")
  }
}

func test_roundtrip() async {
  print("[swift] test_roundtrip")
  let system = PortableRoundtripActorSystem()
  // Local greeter the system can dispatch to when it gets a "remote" call.
  let local = Greeter(actorSystem: system)
  do {
    let remoteRef = try Greeter.resolve(id: local.id, using: system)
    let result = try await remoteRef.hello(name: "World")
    print("[swift] result: \(result)")
  } catch {
    print("[swift] threw")
  }
}
// CHECK-LABEL: [swift] test_roundtrip
// CHECK: [swift] remoteCall reached
// CHECK: [swift] result: Hello, World!

func test_resolvableAny() async {
  print("[swift] test_resolvableAny")
  let system = PortableRoundtripActorSystem()
  let hub = Hub(actorSystem: system)
  let worker = WorkerImpl(actorSystem: system)
  do {
    let remoteHub = try Hub.resolve(id: hub.id, using: system)
    let remoteWorker = try $ResolvableWorker.resolve(id: worker.id, using: system)
    let s = try await remoteHub.dispatch(to: remoteWorker)
    print("[swift] dispatch result: \(s)")
  } catch {
    print("[swift] threw")
  }
}
// CHECK-LABEL: [swift] test_resolvableAny
// CHECK:      [swift] remoteCall reached
// CHECK-NEXT: [swift] remoteCall reached
// CHECK-NEXT: [swift] dispatch result: worked: world


@main struct Main {
  static func main() async {
    await test_roundtrip()
    await test_resolvableAny()
  }
}

