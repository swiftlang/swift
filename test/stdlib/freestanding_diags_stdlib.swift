// RUN: %target-typecheck-verify-swift -concurrency-model=task-to-thread

// REQUIRES: freestanding

import _Concurrency

@MainActor(unsafe) // expected-error{{not permitted within task-to-thread concurrency model}}
func chowMein() async {
}

@MainActor // expected-error{{not permitted within task-to-thread concurrency model}}
class ChowMein {}

func foo() async {
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
  let _: Int = await withCheckedContinuation { _ in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  do {
  let _: Int = try await withCheckedThrowingContinuation { _ in } // expected-error{{Unavailable in task-to-thread concurrency model}}
  } catch let error {
    _ = error
  }
  _ = CheckedContinuation<Int, Never>.self // expected-error{{Unavailable in task-to-thread concurrency model}}
}

func foo2(
    body: @MainActor @Sendable () throws -> () // expected-error{{annotating a type with a global actor 'MainActor' is not permitted within task-to-thread concurrency model}}
) {}
