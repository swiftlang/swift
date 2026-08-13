// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.0-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeNonsendingActorSystems.swiftmodule -module-name FakeNonsendingActorSystems -target %target-swift-6.0-abi-triple -I %t %S/../Inputs/FakeNonsendingActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.0-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift %S/../Inputs/FakeNonsendingActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// A distributed actor generic over its `ActorSystem` cannot inherit the
// system's `remoteCall` isolation at thunk-synthesis time -- the concrete
// system, and therefore the concrete witness, is not yet known. The thunk
// must fall back to `@concurrent nonisolated`, dispatching `remoteCall`
// through the protocol witness table whose slot is plain `@async`.
//
// This test proves that fallback is *sound*: substituting a
// `nonisolated(nonsending)` system into the generic actor still round-trips
// correctly. The optimization simply does not apply -- the thunk hops to the
// generic executor before invoking `remoteCall`, so the witness observes
// `#isolation == nil` rather than the caller's isolation. For comparison a
// concrete distributed actor over the same system observes the caller's
// `MainActor` isolation, showing the optimization *does* fire once the system
// is known.

import Distributed
import FakeDistributedActorSystems
import FakeNonsendingActorSystems

@available(SwiftStdlib 6.0, *)
distributed actor Worker<ActorSystem>
    where ActorSystem: DistributedActorSystem<any Codable> {
  distributed func greet() -> String { "hi" }
  distributed func poke() {}
}

@available(SwiftStdlib 6.0, *)
distributed actor ConcreteWorker {
  typealias ActorSystem = FakeNonsendingRoundtripActorSystem
  distributed func greet() -> String { "hi" }
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  @MainActor
  static func main() async throws {
    let system = FakeNonsendingRoundtripActorSystem()

    // Report what the witness saw. For the generic actor the thunk is
    // `@concurrent`, so the caller has already hopped away by the time we get
    // here and `#isolation` is nil. For the concrete actor the thunk is
    // `nonisolated(nonsending)`, so the caller's isolation is forwarded
    system.onRemoteCall = { isolation in
      if isolation == nil {
        print("remoteCall isolation == nil")
      } else if let iso = isolation, iso === MainActor.shared {
        print("remoteCall isolation == MainActor.shared")
      } else {
        print("remoteCall isolation == other")
      }
    }
    system.stubbedRemoteCallReply = "stubbed"

    // ==== -----------------------------------------------------------------
    // Generic actor: thunk is `@concurrent`, witness sees isolation=nil
    let local = Worker<FakeNonsendingRoundtripActorSystem>(actorSystem: system)
    let remote = try Worker<FakeNonsendingRoundtripActorSystem>.resolve(
        id: local.id, using: system)
    // CHECK: remoteCall isolation == nil
    let g = try await remote.greet()
    // CHECK: generic greet -> stubbed
    print("generic greet -> \(g)")
    // CHECK: remoteCall isolation == nil
    try await remote.poke()

    // ==== -----------------------------------------------------------------
    // Concrete actor over the same system: thunk is `nonisolated(nonsending)`,
    // witness sees the caller's MainActor isolation
    let cLocal = ConcreteWorker(actorSystem: system)
    let cRemote = try ConcreteWorker.resolve(id: cLocal.id, using: system)
    // CHECK: remoteCall isolation == MainActor.shared
    let c = try await cRemote.greet()
    // CHECK: concrete greet -> stubbed
    print("concrete greet -> \(c)")

    // CHECK: OK
    print("OK")
  }
}
