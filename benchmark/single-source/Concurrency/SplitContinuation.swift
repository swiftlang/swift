//===--- SplitContinuation.swift ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(anyAppleOS 9999, *) else { return [] }
  return [
    BenchmarkInfo(name: "SplitWait.Early.None",
                  runFunction: run_SplitWait_Early_None,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Early.Cancel",
                  runFunction: run_SplitWait_Early_Cancel,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Early.Escalate",
                  runFunction: run_SplitWait_Early_Escalate,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Early.Both",
                  runFunction: run_SplitWait_Early_Both,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Early.Around",
                  runFunction: run_SplitWait_Early_Around,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.TaskExec.None",
                  runFunction: run_SplitWait_TaskExec_None,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.TaskExec.Both",
                  runFunction: run_SplitWait_TaskExec_Both,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.TaskExec.Around",
                  runFunction: run_SplitWait_TaskExec_Around,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.SerialExec.None",
                  runFunction: run_SplitWait_SerialExec_None,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.SerialExec.Both",
                  runFunction: run_SplitWait_SerialExec_Both,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.SerialExec.Around",
                  runFunction: run_SplitWait_SerialExec_Around,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.BothExec.None",
                  runFunction: run_SplitWait_BothExec_None,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.BothExec.Both",
                  runFunction: run_SplitWait_BothExec_Both,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.BothExec.Around",
                  runFunction: run_SplitWait_BothExec_Around,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Enqueued.None",
                  runFunction: run_SplitWait_Enqueued_None,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Enqueued.Both",
                  runFunction: run_SplitWait_Enqueued_Both,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "SplitWait.Enqueued.Around",
                  runFunction: run_SplitWait_Enqueued_Around,
                  tags: [.concurrency]),
  ]
}

private let count = 5_000
private let suspendedCount = 2_000

@available(anyAppleOS 9999, *)
@inline(never)
private func run_SplitWait_Early_None(_ n: Int) async {
  for _ in 0..<n {
    var sum = 0
    for i in 0..<count {
      sum &+= await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        continuation.resume(returning: i)
        return await waiter.wait()
      }
    }
    check(sum == count * (count - 1) / 2)
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
private func run_SplitWait_Early_Cancel(_ n: Int) async {
  for _ in 0..<n {
    var sum = 0
    for i in 0..<count {
      sum &+= await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        continuation.resume(returning: i)
        return await waiter.wait(onCancel: { reason in blackHole(reason) })
      }
    }
    check(sum == count * (count - 1) / 2)
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
private func run_SplitWait_Early_Escalate(_ n: Int) async {
  for _ in 0..<n {
    var sum = 0
    for i in 0..<count {
      sum &+= await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        continuation.resume(returning: i)
        return await waiter.wait(onPriorityEscalated: { old, new in
          blackHole(old)
          blackHole(new)
        })
      }
    }
    check(sum == count * (count - 1) / 2)
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
private func run_SplitWait_Early_Both(_ n: Int) async {
  for _ in 0..<n {
    var sum = 0
    for i in 0..<count {
      sum &+= await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        continuation.resume(returning: i)
        return await waiter.wait(
          onCancel: { reason in blackHole(reason) },
          onPriorityEscalated: { old, new in
            blackHole(old)
            blackHole(new)
          })
      }
    }
    check(sum == count * (count - 1) / 2)
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
private func run_SplitWait_Early_Around(_ n: Int) async {
  for _ in 0..<n {
    var sum = 0
    for i in 0..<count {
      sum &+= await withTaskCancellationHandler {
        await withTaskPriorityEscalationHandler {
          await withContinuation(of: Int.self) { continuation in
            continuation.resume(returning: i)
          }
        } onPriorityEscalated: { old, new in
          blackHole(old)
          blackHole(new)
        }
      } onCancel: {
        blackHole(0)
      }
    }
    check(sum == count * (count - 1) / 2)
  }
}


// MARK: Suspended waits, resumed on the waiting task's own thread.

private enum ResumeOffer {
  case taskExecutor
  case serialExecutor
  case bothExecutors
  /// Resumes with a plain `resume`, which enqueues the task on the loop.
  case none
}

private enum WaitKind {
  case noHandlers
  case handlers
  case handlersAround
}

@available(anyAppleOS 9999, *)
private final class ResumeLoop: @unchecked Sendable {
  let offer: ResumeOffer
  lazy var taskExecutor = LoopTaskExecutor(loop: self)
  lazy var serialExecutor = LoopSerialExecutor(loop: self)

  private var jobs: [(job: UnownedJob, onSerialExecutor: Bool)] = []
  private(set) var jobsRun = 0

  var pending: Continuation<Int, Never>?
  var pendingValue = 0

  init(offer: ResumeOffer) {
    self.offer = offer
  }

  func enqueue(_ job: UnownedJob, onSerialExecutor: Bool) {
    jobs.append((job, onSerialExecutor))
  }

  func run() {
    while true {
      if let continuation = pending.take() {
        resume(continuation)
      } else if !jobs.isEmpty {
        let (job, onSerialExecutor) = jobs.removeFirst()
        jobsRun += 1
        if !onSerialExecutor {
          unsafe job.runSynchronously(
            on: taskExecutor.asUnownedTaskExecutor())
        } else if offer == .bothExecutors {
          unsafe job.runSynchronously(
            isolatedTo: serialExecutor.asUnownedSerialExecutor(),
            taskExecutor: taskExecutor.asUnownedTaskExecutor())
        } else {
          unsafe job.runSynchronously(
            on: serialExecutor.asUnownedSerialExecutor())
        }
      } else {
        return
      }
    }
  }

  private func resume(_ continuation: consuming Continuation<Int, Never>) {
    switch offer {
    case .taskExecutor:
      continuation.resumeSynchronously(
        returning: pendingValue, on: taskExecutor.asUnownedTaskExecutor())
    case .serialExecutor:
      continuation.resumeSynchronously(
        returning: pendingValue,
        isolatedTo: serialExecutor.asUnownedSerialExecutor())
    case .bothExecutors:
      continuation.resumeSynchronously(
        returning: pendingValue,
        isolatedTo: serialExecutor.asUnownedSerialExecutor(),
        taskExecutor: taskExecutor.asUnownedTaskExecutor())
    case .none:
      continuation.resume(returning: pendingValue)
    }
  }
}

@available(anyAppleOS 9999, *)
private final class LoopTaskExecutor: TaskExecutor, @unchecked Sendable {
  unowned let loop: ResumeLoop

  init(loop: ResumeLoop) {
    self.loop = loop
  }

  func enqueue(_ job: consuming ExecutorJob) {
    loop.enqueue(UnownedJob(job), onSerialExecutor: false)
  }
}

@available(anyAppleOS 9999, *)
private final class LoopSerialExecutor: SerialExecutor, @unchecked Sendable {
  unowned let loop: ResumeLoop

  init(loop: ResumeLoop) {
    self.loop = loop
  }

  func enqueue(_ job: consuming ExecutorJob) {
    loop.enqueue(UnownedJob(job), onSerialExecutor: true)
  }
}

@available(anyAppleOS 9999, *)
private actor LoopActor {
  let executor: LoopSerialExecutor

  init(_ executor: LoopSerialExecutor) {
    self.executor = executor
  }

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    executor.asUnownedSerialExecutor()
  }

  func waits(_ loop: ResumeLoop, _ wait: WaitKind) async -> Int {
    await suspendedWaits(loop, wait)
  }
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_TaskExec_None(_ n: Int) async {
  await runSuspended(n, offer: .taskExecutor, wait: .noHandlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_TaskExec_Both(_ n: Int) async {
  await runSuspended(n, offer: .taskExecutor, wait: .handlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_TaskExec_Around(_ n: Int) async {
  await runSuspended(n, offer: .taskExecutor, wait: .handlersAround)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_SerialExec_None(_ n: Int) async {
  await runSuspended(n, offer: .serialExecutor, wait: .noHandlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_SerialExec_Both(_ n: Int) async {
  await runSuspended(n, offer: .serialExecutor, wait: .handlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_SerialExec_Around(_ n: Int) async {
  await runSuspended(n, offer: .serialExecutor, wait: .handlersAround)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_BothExec_None(_ n: Int) async {
  await runSuspended(n, offer: .bothExecutors, wait: .noHandlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_BothExec_Both(_ n: Int) async {
  await runSuspended(n, offer: .bothExecutors, wait: .handlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_BothExec_Around(_ n: Int) async {
  await runSuspended(n, offer: .bothExecutors, wait: .handlersAround)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_Enqueued_None(_ n: Int) async {
  await runSuspended(n, offer: .none, wait: .noHandlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_Enqueued_Both(_ n: Int) async {
  await runSuspended(n, offer: .none, wait: .handlers)
}

@available(anyAppleOS 9999, *)
private func run_SplitWait_Enqueued_Around(_ n: Int) async {
  await runSuspended(n, offer: .none, wait: .handlersAround)
}

@available(anyAppleOS 9999, *)
private func runSuspended(_ n: Int, offer: ResumeOffer, wait: WaitKind) async {
  for _ in 0..<n {
    let loop = ResumeLoop(offer: offer)
    let task: Task<Int, Never>
    switch offer {
    case .taskExecutor, .none:
      task = Task(executorPreference: loop.taskExecutor) {
        await suspendedWaits(loop, wait)
      }
    case .serialExecutor:
      let actor = LoopActor(loop.serialExecutor)
      task = Task.immediate { await actor.waits(loop, wait) }
    case .bothExecutors:
      let actor = LoopActor(loop.serialExecutor)
      task = Task(executorPreference: loop.taskExecutor) {
        await actor.waits(loop, wait)
      }
    }
    loop.run()
    let sum = await task.value
    check(sum == suspendedCount * (suspendedCount - 1) / 2)
    check(offer == .none ? loop.jobsRun > suspendedCount : loop.jobsRun <= 3)
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
private nonisolated(nonsending) func suspendedWaits(
  _ loop: ResumeLoop, _ wait: WaitKind
) async -> Int {
  var sum = 0
  switch wait {
  case .noHandlers:
    for i in 0..<suspendedCount {
      sum &+= await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        loop.pending = consume continuation
        loop.pendingValue = i
        return await waiter.wait()
      }
    }
  case .handlers:
    for i in 0..<suspendedCount {
      sum &+= await withContinuation(of: Int.self, throwing: Never.self) {
        continuation, waiter in
        loop.pending = consume continuation
        loop.pendingValue = i
        return await waiter.wait(
          onCancel: { reason in blackHole(reason) },
          onPriorityEscalated: { old, new in
            blackHole(old)
            blackHole(new)
          })
      }
    }
  case .handlersAround:
    for i in 0..<suspendedCount {
      sum &+= await withTaskCancellationHandler {
        await withTaskPriorityEscalationHandler {
          await withContinuation(of: Int.self) { continuation in
            loop.pending = consume continuation
            loop.pendingValue = i
          }
        } onPriorityEscalated: { old, new in
          blackHole(old)
          blackHole(new)
        }
      } onCancel: {
        blackHole(0)
      }
    }
  }
  return sum
}
