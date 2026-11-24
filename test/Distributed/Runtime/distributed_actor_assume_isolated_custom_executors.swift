// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -parse-as-library %import-libdispatch -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// DistributedActor.assumeIsolated on local distributed actors that use a
// custom executor: value, Void and throwing results, assuming from another
// actor that shares the executor, and the remote reference crash

import StdlibUnittest
import Distributed
import Dispatch
import FakeDistributedActorSystems

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

// ==== -----------------------------------------------------------------------
// MARK: Executor

@available(SwiftStdlib 5.9, *)
final class QueueExecutor: SerialExecutor, @unchecked Sendable {
  let queue: DispatchQueue

  init(label: String) {
    self.queue = DispatchQueue(label: label)
  }

  func enqueue(_ job: consuming ExecutorJob) {
    let job = UnownedJob(job)
    queue.async {
      job.runSynchronously(on: self.asUnownedSerialExecutor())
    }
  }
}

// Distributed actor stored properties are not accessible from the nonisolated
// unownedExecutor getter, so the executor lives in a global
@available(SwiftStdlib 5.9, *)
let workerExecutor = QueueExecutor(label: "Worker")

struct Boom: Error, Equatable {
  let value: Int
}

// ==== -----------------------------------------------------------------------
// MARK: Distributed actor with a custom executor

@available(SwiftStdlib 5.9, *)
distributed actor Worker {
  var count = 0
  var name = "worker"

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    workerExecutor.asUnownedSerialExecutor()
  }

  /// Runs on the custom executor; calls into synchronous nonisolated code that
  /// assumes the isolation
  func checkValues() -> (Int, Int, String) {
    (incrementAssumingIsolated(self),
     incrementAssumingIsolated(self),
     nameAssumingIsolated(self))
  }

  func checkRethrow() -> Boom? {
    do {
      _ = try throwAssumingIsolated(self)
      return nil
    } catch let error as Boom {
      return error
    } catch {
      fatalError("unexpected error \(error)")
    }
  }

  func checkVoid() -> Int {
    touchAssumingIsolated(self)
    touchAssumingIsolated(self)
    return count
  }

  func currentCount() -> Int {
    count
  }
}

/// Synchronous and nonisolated: must use assumeIsolated to touch the actor
@available(SwiftStdlib 5.9, *)
func incrementAssumingIsolated(_ worker: Worker) -> Int {
  worker.assumeIsolated { worker in
    worker.count += 1
    return worker.count
  }
}

@available(SwiftStdlib 5.9, *)
func nameAssumingIsolated(_ worker: Worker) -> String {
  worker.assumeIsolated { worker in
    worker.name += "!"
    return worker.name
  }
}

@available(SwiftStdlib 5.9, *)
func touchAssumingIsolated(_ worker: Worker) {
  worker.assumeIsolated { worker in
    worker.count += 10
  }
}

@available(SwiftStdlib 5.9, *)
func throwAssumingIsolated(_ worker: Worker) throws -> Int {
  try worker.assumeIsolated { worker in
    worker.count += 1
    throw Boom(value: worker.count)
  }
}

/// A local actor sharing the distributed actor's custom executor
@available(SwiftStdlib 5.9, *)
actor SharesWorkerExecutor {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    workerExecutor.asUnownedSerialExecutor()
  }

  func increment(_ worker: Worker) -> Int {
    incrementAssumingIsolated(worker)
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Context integrity
//
// assumeIsolated must run the closure in the caller's context, on the expected
// distributed actor, and leave that context intact: same executor (and queue),
// same task, task locals visible, no leaked or over-released captures, and
// executor tracking still correct after a later suspension

enum Marker {
  @TaskLocal static var value = "none"
}

// @unchecked Sendable: captured by actor isolated closures from nonisolated code
final class Canary: @unchecked Sendable {
  var value = 1
}

func currentTaskHash() -> Int? {
  withUnsafeCurrentTask { $0?.hashValue }
}

@available(SwiftStdlib 5.9, *)
func contextCheckAssumingIsolated(_ worker: Worker, task: Int?, canary: Canary) -> Int {
  dispatchPrecondition(condition: .onQueue(workerExecutor.queue))

  let result = worker.assumeIsolated { isolatedWorker in
    isolatedWorker.preconditionIsolated()
    precondition(isolatedWorker === worker, "closure got a different actor")
    dispatchPrecondition(condition: .onQueue(workerExecutor.queue))
    precondition(currentTaskHash() == task, "closure ran on a different task")
    precondition(Marker.value == "outer", "task local not visible in the closure")

    // Nested assumeIsolated on the same distributed actor
    let nested = isolatedWorker.assumeIsolated { again in
      precondition(again === worker)
      dispatchPrecondition(condition: .onQueue(workerExecutor.queue))
      return again.count
    }
    precondition(nested == isolatedWorker.count)

    isolatedWorker.count += canary.value
    return isolatedWorker.count
  }

  worker.preconditionIsolated()
  dispatchPrecondition(condition: .onQueue(workerExecutor.queue))
  precondition(currentTaskHash() == task, "task changed after assumeIsolated")
  precondition(Marker.value == "outer", "task local lost after assumeIsolated")
  return result
}

@available(SwiftStdlib 5.9, *)
extension Worker {
  func checkContextIntegrity() async -> Int {
    let task = currentTaskHash()
    precondition(task != nil)
    var canary = Canary()

    var total = 0
    for _ in 0 ..< 1_000 {
      total = contextCheckAssumingIsolated(self, task: task, canary: canary)
    }
    precondition(isKnownUniquelyReferenced(&canary),
                 "assumeIsolated leaked a reference to a captured object")

    // The error path leaves the context intact too
    do {
      _ = try throwAssumingIsolated(self)
      fatalError("expected a throw")
    } catch {
      self.preconditionIsolated()
      dispatchPrecondition(condition: .onQueue(workerExecutor.queue))
      precondition(currentTaskHash() == task)
    }

    // Suspend and resume: executor tracking must still be correct
    await Task.yield()
    self.preconditionIsolated()
    dispatchPrecondition(condition: .onQueue(workerExecutor.queue))
    precondition(currentTaskHash() == task)

    total = contextCheckAssumingIsolated(self, task: task, canary: canary)
    precondition(isKnownUniquelyReferenced(&canary))
    return total
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Tests

@available(SwiftStdlib 5.9, *)
@main struct Main {
  static func main() async {
    let tests = TestSuite("DistributedAssumeIsolatedCustomExecutors")
    let system = DefaultDistributedActorSystem()

    tests.test("DistributedActor.assumeIsolated: custom executor, value results") {
      let worker = Worker(actorSystem: system)
      let result = await worker.whenLocal { await $0.checkValues() }
      guard let (one, two, name) = result else {
        fatalError("expected a local actor")
      }
      expectEqual(1, one)
      expectEqual(2, two)
      expectEqual("worker!", name)
    }

    tests.test("DistributedActor.assumeIsolated: custom executor, Void result") {
      let worker = Worker(actorSystem: system)
      expectEqual(20, await worker.whenLocal { await $0.checkVoid() })
    }

    tests.test("DistributedActor.assumeIsolated: custom executor, rethrows") {
      let worker = Worker(actorSystem: system)
      let caught = await worker.whenLocal { await $0.checkRethrow() }
      expectEqual(Boom(value: 1), caught ?? nil)
      expectEqual(1, await worker.whenLocal { await $0.currentCount() })
    }

    tests.test("DistributedActor.assumeIsolated: from an actor sharing the custom executor") {
      let worker = Worker(actorSystem: system)
      let friend = SharesWorkerExecutor()
      expectEqual(1, await friend.increment(worker))
      expectEqual(2, await friend.increment(worker))
    }

    tests.test("DistributedActor.assumeIsolated: context integrity on a custom executor") {
      let worker = Worker(actorSystem: system)
      let total = await Marker.$value.withValue("outer") {
        await worker.whenLocal { await $0.checkContextIntegrity() }
      }
      // 1000 increments, one throwing increment, one more increment
      expectEqual(1_002, total)
      expectEqual(1_002, await worker.whenLocal { await $0.currentCount() })
    }

    tests.test("DistributedActor.assumeIsolated: wrongly assume the custom executor")
    .require(.crashTesting)
    .code {
      // A custom executor that does not recognize the current context falls
      // back to its checkIsolated(), whose default implementation crashes
      // before assumeIsolated reports its own message
      expectCrashLater(withMessage: "Unexpected isolation context, expected to be executing on QueueExecutor")
      let worker = Worker(actorSystem: system)
      _ = incrementAssumingIsolated(worker)
    }

    tests.test("DistributedActor.assumeIsolated: on remote actor reference")
    .require(.crashTesting)
    .code {
      expectCrashLater(withMessage: "Cannot assume to be 'isolated Worker' since distributed actor 'a.Worker' is a remote actor reference.")
      let local = Worker(actorSystem: system)
      let remote = try! Worker.resolve(id: local.id, using: system)
      _ = await SharesWorkerExecutor().increment(remote)
    }

    await runAllTestsAsync()
  }
}
