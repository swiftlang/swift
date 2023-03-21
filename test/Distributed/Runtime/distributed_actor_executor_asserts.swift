// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME(distributed): Distributed actors currently have some issues on windows rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor MainWorker: Worker {
  nonisolated var localUnownedExecutor: UnownedSerialExecutor? {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }
}

distributed actor NormalWorker: Worker {
  // empty on purpose, default executor
}

protocol Worker: DistributedActor {
}

extension Worker {
  distributed func preconditionSameExecutor(as other: some Worker) {
    preconditionOnExecutor(of: other, "Expected for [\(self)] share executor with [\(other)]")
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssumeDistributedActorExecutor")

    if #available(SwiftStdlib 5.9, *) {
      let system = DefaultDistributedActorSystem()

      let normalLocalWorker = NormalWorker(actorSystem: system)
      precondition(__isLocalActor(normalLocalWorker), "must be local")

      let normalRemoteWorker = try! NormalWorker.resolve(id: normalLocalWorker.id, using: system)
      precondition(__isRemoteActor(normalRemoteWorker), "must be remote")

      if #available(SwiftStdlib 5.9, *) {
        precondition(normalLocalWorker.id == normalRemoteWorker.id, "IDs must be equal")

        tests.test("exactly the same actor") {
          try! await normalLocalWorker.preconditionSameExecutor(as: normalLocalWorker)
        }

        tests.test("different normal local worker, not same executor") {
          expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected 'UnownedSerialExecutor(executor: (Opaque Value))' executor. Expected for [main.NormalWorker] share executor with main.NormalWorker")
          let other = NormalWorker(actorSystem: system)
          try! await normalLocalWorker.preconditionSameExecutor(as: other)
        }

        tests.test("remote actor reference should have nil executor") {
          precondition(normalRemoteWorker.localUnownedExecutor == nil,
              "Expected nil executor but was: \(String(describing: normalRemoteWorker.localUnownedExecutor!))")
        }
      }
    }

    await runAllTestsAsync()
  }
}
