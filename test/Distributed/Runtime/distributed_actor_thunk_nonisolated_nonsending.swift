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

// End-to-end check that a distributed actor system declaring its `remoteCall`
// witness as `nonisolated(nonsending)` causes the synthesized distributed
// thunks to forward the caller's actor isolation into `remoteCall` (SE-0461),
// rather than hopping to the generic executor first.
//
// Two things must hold for such a thunk:
//
//   * Remote branch: the thunk must not hop off the caller before invoking
//     `remoteCall`. `FakeNonsendingRoundtripActorSystem.onRemoteCall` reports
//     the `#isolation` observed inside the witness; called from `@MainActor`
//     it must be `MainActor.shared`, on the main actor's executor.
//     `stubbedRemoteCallReply` prevents the target function from executing so
//     the only executor changes we could observe are the ones under test
//
//   * Local branch: the thunk must still hop onto the target actor before
//     calling the actual `distributed func`. `preconditionIsolated(self)`
//     inside `greet`/`poke` traps otherwise

import Distributed
import FakeDistributedActorSystems
import FakeNonsendingActorSystems

@available(SwiftStdlib 6.0, *)
distributed actor Greeter {
  typealias ActorSystem = FakeNonsendingRoundtripActorSystem

  var counter = 0

  distributed func greet() -> String {
    // Local branch only. The thunk must have hopped onto self before this
    // executed - a 'nonisolated(nonsending)' thunk must not skip the local hop
    self.preconditionIsolated("local distributed call is not isolated to self")
    counter += 1
    return "local-reply-\(counter)"
  }

  distributed func poke() {
    self.preconditionIsolated("local distributed call is not isolated to self")
    counter += 1
  }
}

@available(SwiftStdlib 6.0, *)
@main struct Main {
  @MainActor
  static func main() async throws {
    let system = FakeNonsendingRoundtripActorSystem()
    system.onRemoteCall = { isolation in
      // Assert the actual executor, not just the isolation value: checking
      // '#isolation' alone can pass while really executing on the target
      // actor's executor
      MainActor.preconditionIsolated("remoteCall lost the caller's isolation")
      if let isolation, isolation === MainActor.shared {
        print("remoteCall isolation == MainActor.shared")
      } else {
        print("remoteCall isolation == \(String(describing: isolation))")
      }
    }

    let local = Greeter(actorSystem: system)

    // ==== -----------------------------------------------------------------
    // Remote branch: must stay on the caller (MainActor), never hop
    system.stubbedRemoteCallReply = "remote-reply" // prevent executing actual real target function
    let remote = try Greeter.resolve(id: local.id, using: system)

    // CHECK: remoteCall isolation == MainActor.shared
    let r1 = try await remote.greet()
    // CHECK: remote greet -> remote-reply
    print("remote greet -> \(r1)")

    // CHECK: remoteCall isolation == MainActor.shared
    try await remote.poke()

    // ==== -----------------------------------------------------------------
    // Local branch: the thunk must hop onto the actor
    system.stubbedRemoteCallReply = nil
    system.resolveLocally = true
    let resolved = try Greeter.resolve(id: local.id, using: system)

    let l1 = try await resolved.greet()
    // CHECK: local greet -> local-reply-1
    print("local greet -> \(l1)")

    try await resolved.poke()
    let l2 = try await resolved.greet()
    // CHECK: local greet -> local-reply-3
    print("local greet -> \(l2)")

    // CHECK: OK
    print("OK")
  }
}
