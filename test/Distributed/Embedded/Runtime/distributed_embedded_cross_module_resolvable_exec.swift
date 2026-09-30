// RUN: %empty-directory(%t)

// Build the reusable fake actor system module.
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo %S/Inputs/EmbeddedFakeActorSystem.swift -module-name EmbeddedFakeActorSystem -emit-module -emit-module-path %t/EmbeddedFakeActorSystem.swiftmodule -c -o %t/EmbeddedFakeActorSystem.o

// Build the `@Resolvable` API module
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -plugin-path %swift-plugin-dir -I %t %S/Inputs/GreeterAPI.swift -module-name GreeterAPI -emit-module -emit-module-path %t/GreeterAPI.swiftmodule -c -o %t/GreeterAPI.o

// Build the server module (only module with concrete `GreeterImpl`):
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -I %t %S/Inputs/GreeterServer.swift -module-name GreeterServer -emit-module -emit-module-path %t/GreeterServer.swiftmodule -c -o %t/GreeterServer.o

// Build the client / main module:
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -I %t %s -module-name main -c -o %t/main.o

// Run client:
// RUN: %target-embedded-link %t/EmbeddedFakeActorSystem.o %t/GreeterAPI.o %t/GreeterServer.o %t/main.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// A realistic client/server split over a `@Resolvable` distributed actor protocol,
// spread across four modules:
//
//   - EmbeddedFakeActorSystem
//   - GreeterAPI
//   - GreeterServer, has GreeterImpl
//   - main

import _Concurrency
import Distributed
import EmbeddedFakeActorSystem
import GreeterAPI
import GreeterServer

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()

    let impl = GreeterImpl(actorSystem: system)
    let id = impl.id

    do {
      let greeter = try $Greeter.resolve(id: id, using: system)

      // Simple types, serialization defined in EmbeddedFakeActorSystem
      print("[swift] hello: \(try await greeter.hello(name: "World"))")
      print("[swift] farewell: \(try await greeter.farewell(name: "World"))")

      // Complex types defined in GreeterAPI (along with serialization)
      let response = try await greeter.check(ComplexRequest(id: 7))
      print("[swift] check response id: \(response.id)")

      // Void
      try await greeter.note("ping")
    } catch {
      print("[swift] threw")
    }
  }
}

// CHECK:      [swift] remoteCall reached
// CHECK-NEXT: [swift] hello: Hello, World!
// CHECK:      [swift] remoteCall reached
// CHECK-NEXT: [swift] farewell: Goodbye, World!
// CHECK:      [swift] remoteCall reached
// CHECK-NEXT: [swift] server checked request id: 7
// CHECK-NEXT: [swift] check response id: 8
// CHECK:      [swift] remoteCallVoid reached
// CHECK-NEXT: [swift] server noted: ping
