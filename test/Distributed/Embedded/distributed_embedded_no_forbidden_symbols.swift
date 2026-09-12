// RUN: %target-swift-frontend -emit-ir -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDistributed -parse-as-library -wmo -target %target-cpu-apple-macos14 %s %S/Runtime/Inputs/EmbeddedFakeActorSystem.swift | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDistributed

// Verify that in Embedded Swift, distributed-actor code does NOT pull in
// the standard runtime entry points that rely on demangling, metadata
// reconstruction, or the global accessible-function table. None of these
// exist in the Embedded runtime; their presence in emitted IR would mean
// link errors at best, and broken codegen at worst.

import _Concurrency
import Distributed

typealias DefaultDistributedActorSystem = EmbeddedFakeRoundtripActorSystem

distributed actor Greeter {
  distributed func hello(name: String) -> String { "hi \(name)" }
}

@main struct Main {
  static func main() async {
    let system = EmbeddedFakeRoundtripActorSystem()
    let greeter = Greeter(actorSystem: system)
    let remote = try! Greeter.resolve(id: greeter.id, using: system)
    _ = try? await greeter.hello(name: "World")
    _ = try? await remote.hello(name: "World")
  }
}

// Sanity: the test actually produced output.
// CHECK: ModuleID

// === Runtime symbols that must NOT appear in Embedded distributed IR ===

// Accessible-function table lookup. Replaced by per-actor accessor.
// CHECK-NOT: swift_findAccessibleFunction

// Runtime mangled-name -> Metadata reconstruction. Embedded has no
// demangler.
// CHECK-NOT: swift_getTypeByMangledNode
// CHECK-NOT: swift_getTypeByMangledName
// CHECK-NOT: swift_func_getParameterCount
// CHECK-NOT: swift_func_getParameterTypeInfo
// CHECK-NOT: swift_func_getReturnTypeInfo

// Runtime protocol-conformance lookup. Not used by distributed dispatch
// in Embedded Swift; all conformances are statically known.
// CHECK-NOT: call {{.*}} @swift_conformsToProtocol(
// CHECK-NOT: call {{.*}} @swift_conformsToProtocol2(

// Distributed-specific witness-table malloc (used for runtime ad-hoc
// witness retrieval) - unused in Embedded Swift.
// CHECK-NOT: swift_distributed_getWitnessTables

// The standard receiver entry point - replaced in Embedded Swift by a
// thinner per-actor dispatch.
// CHECK-NOT: swift_distributed_execute_target
