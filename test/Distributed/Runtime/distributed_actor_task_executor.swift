// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main %import-libdispatch -j2 -parse-as-library -target %target-swift-6.0-abi-triple -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Dispatch
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

final class NaiveQueueExecutor: TaskExecutor {
  let queue: DispatchQueue

  init(_ queue: DispatchQueue) {
    self.queue = queue
  }

  public func enqueue(_ _job: consuming ExecutorJob) {
    let job = UnownedJob(_job)
    queue.async {
      job.runSynchronously(on: self.asUnownedTaskExecutor())
    }
  }

  @inlinable
  public func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }
}

distributed actor Worker {

  let expectedQueue: DispatchQueue

  init(expectedQueue: DispatchQueue, actorSystem: ActorSystem) {
    self.expectedQueue = expectedQueue
    self.actorSystem = actorSystem
  }

  distributed func test(x: Int) throws {
    dispatchPrecondition(condition: .onQueue(expectedQueue))
    print("test: executed on expected queue: \(expectedQueue)")
  }
}

@main struct Main {
  static func main() async {
    let queue = DispatchQueue(label: "example-queue")
    let executor = NaiveQueueExecutor(queue)

    let worker = Worker(expectedQueue: queue, actorSystem: DefaultDistributedActorSystem())
    // CHECK: | assign id
    // CHECK: | actor ready

    await withTaskExecutorPreference(executor) {
      try! await worker.test(x: 42)
      // CHECK: test: executed on expected queue
    }

    print("OK") // CHECK: OK
  }
}
