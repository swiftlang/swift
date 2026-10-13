// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -disable-availability-checking -parse-as-library -o %t/split_continuation_escalation -swift-version 6
// RUN: %target-codesign %t/split_continuation_escalation
// RUN: %target-run %t/split_continuation_escalation | %FileCheck %s

// REQUIRES: VENDOR=apple
// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

// Priority escalation is only delivered where the platform supports it; the
// simulators are excluded for the same reason as async_task_escalate_priority.
// UNSUPPORTED: DARWIN_SIMULATOR=watchos
// UNSUPPORTED: DARWIN_SIMULATOR=ios
// UNSUPPORTED: DARWIN_SIMULATOR=tvos

@_spi(Concurrency) import _Concurrency
import Synchronization

// A task executor that queues jobs instead of running them, so tests can
// step through suspend/resume interleavings deterministically.
final class ManualExecutor: TaskExecutor, Sendable {
  private let jobs = Mutex<[UnownedJob]>([])

  func enqueue(_ job: UnownedJob) {
    jobs.withLock { $0.append(job) }
  }

  func asUnownedTaskExecutor() -> UnownedTaskExecutor {
    UnownedTaskExecutor(ordinary: self)
  }

  private func popJob() -> UnownedJob? {
    jobs.withLock { $0.isEmpty ? nil : $0.removeFirst() }
  }

  // Runs exactly one enqueued job, trapping if none is enqueued. Tests that
  // know exactly how many jobs a step should produce use this (or
  // `runNextJobs(count:)`) so an unexpected missing (or extra, left
  // stranded in the queue) enqueue is caught immediately.
  func runNextJob() {
    guard let job = popJob() else {
      preconditionFailure("runNextJob() called with no job enqueued")
    }
    unsafe job.runSynchronously(on: asUnownedTaskExecutor())
  }

  // Runs exactly `count` enqueued jobs, one at a time -- see `runNextJob()`.
  func runNextJobs(count: Int) {
    for _ in 0..<count { runNextJob() }
  }
}

final class ContinuationHolder<Success: ~Copyable, Failure: Error>: Sendable {
  private let storage: Mutex<Continuation<Success, Failure>?>

  init(_ continuation: consuming Continuation<Success, Failure>) {
    storage = Mutex(continuation)
  }

  func take() -> Continuation<Success, Failure> {
    storage.withLock { $0.take()! }
  }
}

extension ContinuationHolder where Success: Sendable {
  func resumeFromDetachedTask(
    returning value: Success, executorPreference executor: (any TaskExecutor)?
  ) {
    Task.detached(executorPreference: executor) { [self] in
      self.take().resume(returning: value)
    }
  }
}

final class Box<Value: Sendable>: Sendable {
  private let storage: Mutex<Value>
  init(_ value: Value) { storage = Mutex(value) }

  var value: Value {
    get { storage.withLock { $0 } }
    set { storage.withLock { $0 = newValue } }
  }
}

@main struct Main {
  static func main() async {
    let executor = ManualExecutor()
    let observedPriority = Box<TaskPriority?>(nil)

    let task = Task(executorPreference: executor, priority: .background) { () -> Int in
      await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        let holder = ContinuationHolder(continuation)
        return await waiter.wait(
          onCancel: { holder.resumeFromDetachedTask(returning: 7, executorPreference: executor) },
          onEscalate: { newPriority in observedPriority.value = newPriority })
      }
    }

    executor.runNextJob() // The one job: runs the task to its suspension,
                           // installing the handlers.

    // Escalation walks the same status-record list cancellation does, and
    // invokes onEscalate synchronously on the calling thread -- no need to
    // wait for or race a real background thread.
    task.escalatePriority(to: .medium)

    // The cancellation handler is still installed, so cancelling now
    // resumes the continuation.
    task.cancel()
    executor.runNextJobs(count: 2) // The cancellation handler's resume, then
                                    // the continuation resuming.

    let result = await task.value
    precondition(result == 7, "continuation did not resume: \(result)")
    precondition(observedPriority.value == .medium,
                 "escalation handler saw \(String(describing: observedPriority.value))")

    // The same with only an escalation handler installed.
    let escalationOnlyExecutor = ManualExecutor()
    let escalationOnlyPriority = Box<TaskPriority?>(nil)
    let handOff = Mutex<Continuation<Int, Never>?>(nil)

    let escalationOnlyTask = Task(executorPreference: escalationOnlyExecutor, priority: .background) { () -> Int in
      await withContinuation(of: Int.self, throwing: Never.self) {
        (continuation: consuming Continuation<Int, Never>,
         waiter: consuming ContinuationWaiter<Int, Never>) in
        var continuation = Optional(continuation)
        handOff.withLock { $0 = continuation.take() }
        return await waiter.wait(
          onEscalate: { newPriority in escalationOnlyPriority.value = newPriority })
      }
    }

    escalationOnlyExecutor.runNextJob() // Runs the task to its suspension.
    escalationOnlyTask.escalatePriority(to: .medium)
    handOff.withLock { $0.take()! }.resume(returning: 8)
    escalationOnlyExecutor.runNextJob()

    let escalationOnlyResult = await escalationOnlyTask.value
    precondition(escalationOnlyResult == 8,
                 "continuation did not resume: \(escalationOnlyResult)")
    precondition(escalationOnlyPriority.value == .medium,
                 "escalation handler saw \(String(describing: escalationOnlyPriority.value))")
    // CHECK: OK
    print("OK")
  }
}
