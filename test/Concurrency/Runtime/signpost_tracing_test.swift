// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -g %t/main.swift %S/Inputs/SignpostMonitor.swift -o %t/a.out -target %target-swift-6.2-abi-triple -Onone
// RUN: %target-codesign %t/a.out
// RUN: env %env-SWIFT_CONCURRENCY_TRACING_SUBSYSTEM=org.swift.concurrency.test.tracing %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: OS=macosx
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Verify signposts emitted by the Concurrency runtime. Built at -Onone to avoid
// the optimizer eliminating unnecessary Concurrency-related operations.

import Foundation
import os
import StdlibUnittest

// MARK: - Tests

var tests = TestSuite("SignpostTracing")

tests.test("BasicTaskLifecycle") {
  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  let value = await Task { return 42 }.value
  precondition(value == 42)

  await monitor.expectAllSignpostsReceived(
    beginEnd("task_lifetime")
    + [ExpectedSignpost(name: "job_run", type: .begin,
                        messageContains: "executorKind=0"),
       ExpectedSignpost(name: "job_run", type: .end),
       ExpectedSignpost(name: "job_enqueue_global", type: .event),
       ExpectedSignpost(name: "task_status_changed", type: .event)]
  )
}

tests.test("TaskWait") {
  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  async let result = { () async -> Int in
    try? await Task.sleep(for: .milliseconds(10))
    return 42
  }()
  let _ = await result

  await monitor.expectAllSignpostsReceived(
    beginEnd("task_wait")
  )
}

tests.test("Continuation") {
  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  await withCheckedContinuation {
    (continuation: CheckedContinuation<Void, Never>) in
    DispatchQueue.global().async {
      continuation.resume()
    }
  }

  await monitor.expectAllSignpostsReceived(
    beginEnd("task_continuation")
    + [ExpectedSignpost(name: "task_continuation_await", type: .event)]
  )
}

tests.test("ThrowingContinuationWithError") {
  struct TestError: Error {}

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  do {
    try await withCheckedThrowingContinuation {
      (continuation: CheckedContinuation<Void, Error>) in
      DispatchQueue.global().async {
        continuation.resume(throwing: TestError())
      }
    }
  } catch {}

  await monitor.expectAllSignpostsReceived(
    beginEnd("task_continuation")
    + [ExpectedSignpost(name: "task_continuation_await", type: .event)]
  )
}

// Tests the throwing continuation path where resume succeeds (no error thrown).
// This exercises swift_continuation_throwingResumeImpl (a different call site
// from the non-throwing and error paths).
tests.test("ThrowingContinuationWithoutError") {
  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  let value = try! await withCheckedThrowingContinuation {
    (continuation: CheckedContinuation<Int, Error>) in
    DispatchQueue.global().async {
      continuation.resume(returning: 42)
    }
  }
  precondition(value == 42)

  await monitor.expectAllSignpostsReceived(
    beginEnd("task_continuation")
    + [ExpectedSignpost(name: "task_continuation_await", type: .event)]
  )
}

// Note: job_enqueue_global_with_delay is not tested because Task.sleep now
// routes through the scheduling executor's enqueue(after:clock:) method
// rather than swift_task_enqueueGlobalWithDelay.

tests.test("MainExecutorEnqueue") {
  @MainActor func onMain() -> Int { return 1 }

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  let _ = await Task.detached {
    return await onMain()
  }.value

  // Hopping to @MainActor goes through swift_task_enqueueImpl, which emits
  // job_enqueue_executor. executorKind=2 is MainActor.
  await monitor.expectAllSignpostsReceived(
    [ExpectedSignpost(name: "job_enqueue_executor", type: .event,
                      messageContains: "executorKind=2"),
     ExpectedSignpost(name: "job_run", type: .begin,
                      messageContains: "executorKind=2")]
  )
}

tests.test("ActorInteraction") {
  actor WorkActor {
    func work() {}
  }

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  let a = WorkActor()
  await a.work()
  withExtendedLifetime(a) {}

  await monitor.expectAllSignpostsReceived(
    [ExpectedSignpost(name: "actor_lifetime", type: .begin),
     ExpectedSignpost(name: "actor_state_changed", type: .event),
     ExpectedSignpost(name: "actor_enqueue", type: .event),
     ExpectedSignpost(name: "actor_dequeue", type: .event),
     ExpectedSignpost(name: "actor_job_queue", type: .event),
     // The job runs on the default actor. executorKind=1 is DefaultActor.
     ExpectedSignpost(name: "job_run", type: .begin,
                      messageContains: "executorKind=1")]
  )
}

tests.test("ActorDeallocation") {
  actor ShortLivedActor {
    func ping() {}
  }

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  // Create and destroy inside a helper so the actor goes out of scope.
  func createAndDestroy() async {
    let a = ShortLivedActor()
    await a.ping()
  }
  await createAndDestroy()

  await monitor.expectAllSignpostsReceived(
    beginEnd("actor_lifetime")
    + [ExpectedSignpost(name: "actor_deallocate", type: .event)]
  )
}

tests.test("TaskGroup") {
  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  await withTaskGroup(of: Int.self) { group in
    group.addTask { 1 }
    group.addTask { 2 }
    var sum = 0
    for await val in group { sum += val }
    precondition(sum == 3)
  }

  await monitor.expectAllSignpostsReceived(
    // At least two child task lifecycles.
    beginEnd("task_lifetime")
    + beginEnd("task_lifetime")
    + [ExpectedSignpost(name: "job_enqueue_global", type: .event),
       ExpectedSignpost(name: "job_enqueue_global", type: .event)]
  )
}

tests.test("AsyncLet") {
  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }

  @Sendable func compute() async -> Int {
    try? await Task.sleep(for: .milliseconds(10))
    return 7
  }
  async let result = compute()
  let _ = await result

  await monitor.expectAllSignpostsReceived(
    beginEnd("task_lifetime")
    + beginEnd("task_wait")
    + [ExpectedSignpost(name: "task_flags_changed", type: .event)]
  )
}

// Test executorKind for a custom serial executor (executorKind=3).
tests.test("CustomSerialExecutorKind") {
  final class MyExecutor: SerialExecutor {
    func enqueue(_ job: consuming ExecutorJob) {
      job.runSynchronously(on: asUnownedSerialExecutor())
    }
  }

  actor CustomExecutorActor {
    let executor: MyExecutor
    nonisolated var unownedExecutor: UnownedSerialExecutor {
      executor.asUnownedSerialExecutor()
    }
    init(executor: MyExecutor) { self.executor = executor }
    func work() -> Int { return 1 }
  }

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }
  let executor = MyExecutor()
  let actor = CustomExecutorActor(executor: executor)

  let _ = await actor.work()

  // executorKind=3 is CustomSerialExecutor.
  await monitor.expectAllSignpostsReceived(
    [ExpectedSignpost(name: "job_enqueue_executor", type: .event,
                      messageContains: "executorKind=3"),
     ExpectedSignpost(name: "job_run", type: .begin,
                      messageContains: "executorKind=3")]
  )
}

// Test the task_switch_executor signpost, emitted when a task hops
// executors without suspending (inside a single job_run interval).
tests.test("TaskSwitchExecutor") {
  actor Inner {
    func work() -> Int { return 1 }
  }
  actor Outer {
    let inner: Inner
    init(inner: Inner) { self.inner = inner }
    func callInner() async -> Int { return await inner.work() }
  }

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }
  let inner = Inner()
  let outer = Outer(inner: inner)

  let _ = await outer.callInner()

  await monitor.expectAllSignpostsReceived(
    [ExpectedSignpost(name: "task_switch_executor", type: .event)]
  )
}

// Test executorKind for a task executor (executorKind=4).
tests.test("TaskExecutorKind") {
  final class MyTaskExecutor: TaskExecutor {
    func enqueue(_ job: consuming ExecutorJob) {
      job.runSynchronously(on: asUnownedTaskExecutor())
    }
  }

  guard let monitor = await SignpostMonitor.start() else {
    print("skipping: signpost log streaming unavailable in this environment")
    return
  }
  let executor = MyTaskExecutor()

  let _ = await Task(executorPreference: executor) {
    return 1
  }.value

  // executorKind=4 is TaskExecutor.
  await monitor.expectAllSignpostsReceived(
    [ExpectedSignpost(name: "job_run", type: .begin,
                      messageContains: "executorKind=4")]
  )
}

await runAllTestsAsync()
