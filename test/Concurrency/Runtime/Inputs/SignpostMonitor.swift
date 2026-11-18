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

  /// How long to wait for `log stream`'s header banner. This banner needs no
  /// daemon round-trip and appears near-instantly when logging is reachable,
  /// so a short budget lets an unavailable environment bail out fast instead
  /// of hanging on the full connection timeout.
  private static let headerTimeout: Duration = .seconds(3)

  private let decoder = JSONDecoder()

  private init(process: Process,
               events: AsyncThrowingStream<SignpostEvent, Error>,
               continuation: AsyncThrowingStream<SignpostEvent, Error>.Continuation) {
    self.process = process
    self.events = events
    self.eventsContinuation = continuation
  }

  /// Launches `log stream`, waits for it to be fully connected (via a canary
  /// signpost), and returns a monitor ready to receive events. Returns nil if
  /// the log stream cannot become ready — e.g. in a restricted execution
  /// environment where the spawned `log` subprocess cannot connect to the
  /// logging daemon. Callers should skip signpost verification in that case.
  ///
  /// Prefer start(), which consults a one-time availability probe so that an
  /// unavailable environment does not pay a connection timeout per test.
  private static func connect() async -> SignpostMonitor? {
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

    // Wait for the header line. `log` prints this banner (echoing its filter
    // predicate) essentially immediately when logging is reachable -- it needs
    // no daemon round-trip -- so a short timeout doubles as a fast availability
    // pre-check: in a restricted environment the subprocess emits nothing and
    // we bail out in a few seconds rather than the full 20s connection budget.
    do {
      try await withThrowingTaskGroup(of: Void.self) { group in
        group.addTask {
          for try await _ in headerReady.0 {}
        }
        group.addTask {
          try await Task.sleep(for: headerTimeout)
          throw TimeoutError()
        }
        try await group.next()
        group.cancelAll()
      }
    } catch {
      // The log stream never produced its header. This happens in restricted
      // execution environments where the `log` subprocess cannot reach the
      // logging daemon. Treat it as "unavailable" so the caller can skip.
      process.terminate()
      return nil
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
      // The stream produced a header but never delivered our canary signpost,
      // so it is not actually live. Treat as unavailable and skip.
      process.terminate()
      return nil
    }
    monitor.canaryReady = nil

    return monitor
  }

  /// Whether `log stream` can be connected in this environment. Probed once
  /// on first use via a short header-timeout pre-check; if that fails
  /// (restricted sandbox, no logging daemon, ...), later tests skip
  /// immediately instead of each re-probing.
  private static var availability: Bool?

  /// Returns a ready monitor, or nil if signpost streaming is unavailable in
  /// this environment. The unavailable verdict is cached so only the first
  /// call pays the (short) availability pre-check.
  static func start() async -> SignpostMonitor? {
    if availability == false {
      return nil
    }
    let monitor = await connect()
    availability = (monitor != nil)
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
