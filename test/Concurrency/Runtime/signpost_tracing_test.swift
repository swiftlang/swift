// RUN: %empty-directory(%t)
// RUN: %target-build-swift -g %s -o %t/a.out -target %target-swift-6.2-abi-triple -Onone
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

// MARK: - Signpost JSON Model

/// Represents one NDJSON record from `log stream --style ndjson`.
struct LogStreamRecord: Decodable {
  var signpostName: String?
  var signpostType: SignpostType?
  var eventMessage: String?

  enum SignpostType: String, Decodable {
    case begin
    case end
    case event
  }
}

// MARK: - Signpost Event

struct SignpostEvent: CustomStringConvertible {
  var name: String
  var type: LogStreamRecord.SignpostType
  var message: String

  var description: String { "\(name) (\(type.rawValue)): \(message)" }
}

// MARK: - Expected Signpost

/// An expected signpost. If `type` is nil, matches any event type with that
/// name. If `messageContains` is non-nil, the event message must contain
/// that substring.
struct ExpectedSignpost: CustomStringConvertible {
  var name: String
  var type: LogStreamRecord.SignpostType?
  var messageContains: String?

  var description: String {
    var result = name
    if let type { result += " (\(type.rawValue))" }
    if let messageContains { result += " [contains: \(messageContains)]" }
    return result
  }

  func matches(_ event: SignpostEvent) -> Bool {
    if name != event.name { return false }
    if let type, type != event.type { return false }
    if let messageContains, !event.message.contains(messageContains) {
      return false
    }
    return true
  }
}

func beginEnd(_ name: String) -> [ExpectedSignpost] {
  [
    ExpectedSignpost(name: name, type: .begin),
    ExpectedSignpost(name: name, type: .end),
  ]
}

// MARK: - Errors

struct TimeoutError: Error, CustomStringConvertible {
  var description: String { "Timed out waiting for signpost events" }
}

// MARK: - Signpost Monitor

/// Monitors signpost events from this process using `log stream`.
///
/// Usage:
///   let monitor = try await SignpostMonitor.start(expected: [...])
///   // ... do async work that triggers signposts ...
///   await monitor.expectAllSignpostsReceived()
class SignpostMonitor {
  private let process: Process
  private let events: AsyncThrowingStream<SignpostEvent, Error>
  private let eventsContinuation: AsyncThrowingStream<SignpostEvent, Error>.Continuation

  static let subsystem = "org.swift.concurrency.test.tracing"
  private static let canaryName = "signpost_monitor_canary"
  private static let canarySignposter = OSSignposter(
    subsystem: SignpostMonitor.subsystem, category: "TestCanary")

  private let decoder = JSONDecoder()

  private init(process: Process,
               events: AsyncThrowingStream<SignpostEvent, Error>,
               continuation: AsyncThrowingStream<SignpostEvent, Error>.Continuation) {
    self.process = process
    self.events = events
    self.eventsContinuation = continuation
  }

  /// Launches `log stream`, waits for it to be fully connected (via a canary
  /// signpost), and returns a monitor ready to receive events.
  static func start() async -> SignpostMonitor {
    let pid = ProcessInfo.processInfo.processIdentifier

    let (events, continuation) = AsyncThrowingStream<SignpostEvent, Error>.makeStream()

    let process = Process()
    let pipe = Pipe()

    process.executableURL = URL(fileURLWithPath: "/usr/bin/log")
    process.arguments = [
      "stream",
      "--signpost",
      "--predicate",
      "subsystem == \"\(subsystem)\" AND processIdentifier == \(pid)",
      "--style", "ndjson",
    ]
    process.standardOutput = pipe
    process.standardError = FileHandle.nullDevice

    let monitor = SignpostMonitor(
      process: process, events: events, continuation: continuation)

    // readabilityHandler is not called concurrently, so no locking needed.
    // Use nonisolated(unsafe) to tell the compiler we know what we're doing.
    nonisolated(unsafe) var buffer = Data()
    let headerReady = AsyncThrowingStream<Void, Error>.makeStream()

    pipe.fileHandleForReading.readabilityHandler = { handle in
      let data = handle.availableData
      guard !data.isEmpty else { return }

      buffer.append(data)

      while let newlineIndex = buffer.firstIndex(of: UInt8(ascii: "\n")) {
        let lineData = buffer[buffer.startIndex..<newlineIndex]
        buffer = buffer[(buffer.index(after: newlineIndex))...]

        guard let line = String(data: lineData, encoding: .utf8) else {
          continue
        }

        // The first line from `log stream` is a header like "Filtering the
        // log data using ...". Use it as the readiness signal.
        if line.hasPrefix("Filtering") || line.hasPrefix("Timestamp") {
          headerReady.1.finish()
          continue
        }

        monitor.processLine(line)
      }
    }

    do {
      try process.run()
    } catch {
      fatalError("Failed to launch log stream: \(error)")
    }

    // Wait for the header line, with timeout.
    do {
      try await withThrowingTaskGroup(of: Void.self) { group in
        group.addTask {
          for try await _ in headerReady.0 {}
        }
        group.addTask {
          try await Task.sleep(for: .seconds(20))
          throw TimeoutError()
        }
        try await group.next()
        group.cancelAll()
      }
    } catch {
      fatalError("log stream failed to become ready: \(error)")
    }

    // The header line appears before the stream is fully connected to logd.
    // Emit canary signposts until we see one come back, confirming the stream
    // is truly live.
    let canaryStream = AsyncThrowingStream<Void, Error>.makeStream()
    monitor.canaryReady = canaryStream.1

    do {
      try await withThrowingTaskGroup(of: Void.self) { group in
        group.addTask {
          for try await _ in canaryStream.0 {}
        }
        group.addTask {
          while !Task.isCancelled {
            canarySignposter.emitEvent("signpost_monitor_canary")
            try await Task.sleep(for: .milliseconds(100))
          }
        }
        group.addTask {
          try await Task.sleep(for: .seconds(20))
          throw TimeoutError()
        }
        try await group.next()
        group.cancelAll()
      }
    } catch {
      fatalError("log stream canary failed: \(error)")
    }
    monitor.canaryReady = nil

    return monitor
  }

  private var canaryReady: AsyncThrowingStream<Void, Error>.Continuation?

  private func processLine(_ line: String) {
    guard let data = line.data(using: .utf8),
          let record = try? decoder.decode(LogStreamRecord.self, from: data),
          let name = record.signpostName
    else {
      return
    }

    // Canary signpost — used to confirm log stream is fully connected.
    if name == SignpostMonitor.canaryName {
      canaryReady?.finish()
      return
    }

    let event = SignpostEvent(
      name: name,
      type: record.signpostType ?? .event,
      message: record.eventMessage ?? ""
    )
    eventsContinuation.yield(event)
  }

  /// Wait for all expected signposts to arrive. On timeout, reports which
  /// signposts were missing and what was observed.
  func expectAllSignpostsReceived(
    _ expected: [ExpectedSignpost],
    timeout: Duration = .seconds(20)
  ) async {
    var remaining = expected
    var observed: [SignpostEvent] = []

    let timeoutTask = Task.detached { [eventsContinuation] in
      try await Task.sleep(for: timeout)
      eventsContinuation.finish(throwing: TimeoutError())
    }

    do {
      for try await event in events {
        observed.append(event)
        if let idx = remaining.firstIndex(where: { $0.matches(event) }) {
          remaining.remove(at: idx)
          if remaining.isEmpty {
            break
          }
        }
      }
    } catch is TimeoutError {
      print("Timed out waiting for signposts. Observed so far:")
      for event in observed {
        print("  \(event)")
      }
      // Fall through to report missing signposts below.
    } catch {
      expectTrue(false, "Unexpected error: \(error)")
    }

    timeoutTask.cancel()
    process.terminate()
    process.waitUntilExit()

    if !remaining.isEmpty {
      var msg = "Missing signposts:\n"
      for m in remaining {
        msg += "  - \(m)\n"
      }
      expectTrue(false, msg)
    }
  }
}

// MARK: - Tests

var tests = TestSuite("SignpostTracing")

tests.test("BasicTaskLifecycle") {
  let monitor = await SignpostMonitor.start()

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
  let monitor = await SignpostMonitor.start()

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
  let monitor = await SignpostMonitor.start()

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

  let monitor = await SignpostMonitor.start()

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
  let monitor = await SignpostMonitor.start()

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

  let monitor = await SignpostMonitor.start()

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

  let monitor = await SignpostMonitor.start()

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

  let monitor = await SignpostMonitor.start()

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
  let monitor = await SignpostMonitor.start()

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
  let monitor = await SignpostMonitor.start()

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

  let monitor = await SignpostMonitor.start()
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

// Test executorKind for a task executor (executorKind=4).
tests.test("TaskExecutorKind") {
  final class MyTaskExecutor: TaskExecutor {
    func enqueue(_ job: consuming ExecutorJob) {
      job.runSynchronously(on: asUnownedTaskExecutor())
    }
  }

  let monitor = await SignpostMonitor.start()
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
