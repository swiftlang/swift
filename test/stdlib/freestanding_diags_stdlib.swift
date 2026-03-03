// RUN: %target-typecheck-verify-swift -concurrency-model=task-to-thread

// REQUIRES: freestanding

import _Concurrency

@MainActor(unsafe) // expected-error{{not permitted within task-to-thread concurrency model}}
func chowMein() async {
}

@MainActor // expected-error{{not permitted within task-to-thread concurrency model}}
class ChowMein {}

@available(SwiftStdlib 5.1, *)
func foo() async throws {
  Task<Void, Never> {} // expected-error{{Unavailable in task-to-thread concurrency model}}
  Task<Void, Error> {} // expected-error{{Unavailable in task-to-thread concurrency model}}
  Task<Void, Never>.detached {} // expected-error{{Unavailable in task-to-thread concurrency model}}
  Task<Void, Error>.detached {} // expected-error{{Unavailable in task-to-thread concurrency model}}
  Task<Void, Error>.runDetached {} // expected-error{{Unavailable in task-to-thread concurrency model}}
  detach { () async -> () in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  detach { () async throws -> () in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  async { () async -> () in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  async { () async throws -> () in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  asyncDetached { () async -> () in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  asyncDetached { () async throws -> () in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  _ = MainActor.self // expected-error{{Unavailable in task-to-thread concurrency model}}
  await Task.sleep(1 as UInt64) // expected-error{{Unavailable in task-to-thread concurrency model}}
  try await Task.sleep(nanoseconds: 1 as UInt64) // expected-error{{Unavailable in task-to-thread concurrency model}}
  _ = AsyncStream<Int>.self // expected-error{{Unavailable in task-to-thread concurrency model}}
  _ = AsyncThrowingStream<Int, Error>.self // expected-error{{Unavailable in task-to-thread concurrency model}}
  func withTaskGroup(_ tg: inout TaskGroup<Int>) async throws {
    tg.addTask(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    tg.addTask { return 1 } // ok
    _ = tg.addTaskUnlessCancelled(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = tg.addTaskUnlessCancelled { return 1 } // ok

    _ = await tg.add(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = await tg.add { return 1 } // expected-warning{{'add(operation:)' is deprecated: renamed to 'addTaskUnlessCancelled(operation:)'}}{{documentation-file=deprecated-declaration}}
                                  // expected-note@-1{{use 'addTaskUnlessCancelled(operation:)' instead}}
    tg.spawn(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    tg.spawn { return 1 } // expected-warning{{'spawn(operation:)' is deprecated: renamed to 'addTask(operation:)'}}{{documentation-file=deprecated-declaration}}
                          // expected-note@-1{{use 'addTask(operation:)' instead}}
    _ = tg.spawnUnlessCancelled(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = tg.spawnUnlessCancelled { return 1 } // expected-warning{{'spawnUnlessCancelled(operation:)' is deprecated: renamed to 'addTaskUnlessCancelled(operation:)'}}{{documentation-file=deprecated-declaration}}
                                             // expected-note@-1{{use 'addTaskUnlessCancelled(operation:)' instead}}
    tg.async(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    tg.async { return 1 } // expected-warning{{'async(operation:)' is deprecated: renamed to 'addTask(operation:)'}}{{documentation-file=deprecated-declaration}}
                          // expected-note@-1{{use 'addTask(operation:)' instead}}
    _ = tg.asyncUnlessCancelled(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = tg.asyncUnlessCancelled { return 1 } // expected-warning{{'asyncUnlessCancelled(operation:)' is deprecated: renamed to 'addTaskUnlessCancelled(operation:)'}}{{documentation-file=deprecated-declaration}}
                                             // expected-note@-1{{use 'addTaskUnlessCancelled(operation:)' instead}}
  }
  func withThrowingTaskGroup(_ tg: inout ThrowingTaskGroup<Int, Error>) async throws {
    tg.addTask(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    tg.addTask { return 1 } // ok
    _ = tg.addTaskUnlessCancelled(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = tg.addTaskUnlessCancelled { return 1 } // ok

    _ = await tg.add(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = await tg.add { return 1 } // expected-warning{{'add(operation:)' is deprecated: renamed to 'addTaskUnlessCancelled(operation:)'}}{{documentation-file=deprecated-declaration}}
                                  // expected-note@-1{{use 'addTaskUnlessCancelled(operation:)' instead}}
    tg.spawn(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    tg.spawn { return 1 } // expected-warning{{'spawn(operation:)' is deprecated: renamed to 'addTask(operation:)'}}{{documentation-file=deprecated-declaration}}
                          // expected-note@-1{{use 'addTask(operation:)' instead}}
    _ = tg.spawnUnlessCancelled(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = tg.spawnUnlessCancelled { return 1 } // expected-warning{{'spawnUnlessCancelled(operation:)' is deprecated: renamed to 'addTaskUnlessCancelled(operation:)'}}{{documentation-file=deprecated-declaration}}
                                             // expected-note@-1{{use 'addTaskUnlessCancelled(operation:)' instead}}
    tg.async(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    tg.async { return 1 } // expected-warning{{'async(operation:)' is deprecated: renamed to 'addTask(operation:)'}}{{documentation-file=deprecated-declaration}}
                          // expected-note@-1{{use 'addTask(operation:)' instead}}
    _ = tg.asyncUnlessCancelled(priority: .low) { return 1 } // expected-error{{Unavailable in task-to-thread concurrency model}}
    _ = tg.asyncUnlessCancelled { return 1 } // expected-warning{{'asyncUnlessCancelled(operation:)' is deprecated: renamed to 'addTaskUnlessCancelled(operation:)'}}{{documentation-file=deprecated-declaration}}
                                             // expected-note@-1{{use 'addTaskUnlessCancelled(operation:)' instead}}
  }
}

@available(SwiftStdlib 5.7, *)
func bar() async {
  func withContinuousClock(_ clock: ContinuousClock) async throws { try await clock.sleep(until: { fatalError() }()) } // expected-error{{Unavailable in task-to-thread concurrency model}}
  func withSuspendingClock(_ clock: SuspendingClock) async throws { try await clock.sleep(until: { fatalError() }()) } // expected-error{{Unavailable in task-to-thread concurrency model}}
  func withClock<C : Clock>(
    until deadline: C.Instant,
    tolerance: C.Instant.Duration? = nil,
    clock: C
  ) async throws {
    try await Task.sleep(until: deadline, tolerance: tolerance, clock: clock) // expected-error{{Unavailable in task-to-thread concurrency model}}
  }
  func withDuration(_ duration: Duration) async throws { try await Task.sleep(for: duration) } // expected-error{{Unavailable in task-to-thread concurrency model}}
}

func foo2(
    body: @MainActor @Sendable () throws -> () // expected-error{{annotating a type with a global actor 'MainActor' is not permitted within task-to-thread concurrency model}}
) {}
