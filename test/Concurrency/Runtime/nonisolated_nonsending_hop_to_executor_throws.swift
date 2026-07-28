// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-upcoming-feature NonisolatedNonsendingByDefault) | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// UNSUPPORTED: freestanding

// Regression tests for: https://github.com/swiftlang/swift/issues/88259
//
// Verifies that nonisolated(nonsending) functions that throw correctly
// hop back to the caller's executor before exiting.

protocol ErrorFactory: Error {
  static func make() -> Self
}

struct SomeError: ErrorFactory {
  static func make() -> SomeError { SomeError() }
}

@concurrent func asyncThrows_throw() async throws {
  throw SomeError()
}

@concurrent func asyncTypedThrows_throw() async throws(SomeError) {
  throw SomeError()
}

@concurrent func asyncTypedThrowsGeneric_throw<E: ErrorFactory>(_: E.Type = E.self) async throws(E) {
  throw E.make()
}

@concurrent func asyncThrows_nothrow() async throws {}

@concurrent func asyncTypedThrows_nothrow() async throws(SomeError) {}

@concurrent func asyncTypedThrowsGeneric_nothrow<E: Error>(_: E.Type = E.self) async throws(E) {}

// MARK: - forwarding wrappers

func callThrows_throw() async throws {
  try await asyncThrows_throw()
}

func callTypedThrows_throw() async throws(SomeError) {
  try await asyncTypedThrows_throw()
}

func callGenericTypedThrows_throw<E: ErrorFactory>(_ e: E.Type = E.self) async throws(E) {
  try await asyncTypedThrowsGeneric_throw(e)
}

func callThrows_nothrow() async throws {
  try await asyncThrows_nothrow()
}

func callTypedThrows_nothrow() async throws(SomeError) {
  try await asyncTypedThrows_nothrow()
}

func callGenericTypedThrows_nothrow<E: Error>(_ e: E.Type = E.self) async throws(E) {
  try await asyncTypedThrowsGeneric_nothrow(e)
}

// MARK: - Tests

@MainActor
func test_callThrows_throw() async {
  // CHECK: callThrows_throw: caught on main
  do {
    try await callThrows_throw()
    fatalError("should have thrown")
  } catch {
    MainActor.preconditionIsolated("should be on main")
    print("callThrows_throw: caught on main")
  }
}

@MainActor
func test_callTypedThrows_throw() async {
  // CHECK: callTypedThrows_throw: caught on main
  do {
    try await callTypedThrows_throw()
    fatalError("should have thrown")
  } catch {
    MainActor.preconditionIsolated("should be on main")
    print("callTypedThrows_throw: caught on main")
  }
}

@MainActor
func test_callGenericTypedThrows_throw() async {
  // CHECK: callGenericTypedThrows_throw: caught on main
  do {
    try await callGenericTypedThrows_throw(SomeError.self)
    fatalError("should have thrown")
  } catch {
    MainActor.preconditionIsolated("should be on main")
    print("callGenericTypedThrows_throw: caught on main")
  }
}

@MainActor
func test_callThrows_nothrow() async {
  // CHECK: callThrows_nothrow: return on main
  do {
    try await callThrows_nothrow()
    MainActor.preconditionIsolated("should be on main")
    print("callThrows_nothrow: return on main")
  } catch {
    fatalError("should not throw")
  }
}

@MainActor
func test_callTypedThrows_nothrow() async {
  // CHECK: callTypedThrows_nothrow: return on main
  do {
    try await callTypedThrows_nothrow()
    MainActor.preconditionIsolated("should be on main")
    print("callTypedThrows_nothrow: return on main")
  } catch {
    fatalError("should not throw")
  }
}

@MainActor
func test_callGenericTypedThrows_nothrow() async {
  // CHECK: callGenericTypedThrows_nothrow: return on main
  do {
    try await callGenericTypedThrows_nothrow(SomeError.self)
    MainActor.preconditionIsolated("should be on main")
    print("callGenericTypedThrows_nothrow: return on main")
  } catch {
    fatalError("should not throw")
  }
}

@MainActor func run() async {
  await test_callThrows_throw()
  await test_callTypedThrows_throw()
  await test_callGenericTypedThrows_throw()
  await test_callThrows_nothrow()
  await test_callTypedThrows_nothrow()
  await test_callGenericTypedThrows_nothrow()
}

await run()
