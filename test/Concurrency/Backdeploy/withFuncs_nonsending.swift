// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx27.0 -module-name main -emit-ir -o %t/new.ir
// RUN: %FileCheck %s --check-prefix=NEW < %t/new.ir
// RUN: %FileCheck %s --check-prefix=NEW-NO-WEAK-REF < %t/new.ir

// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx15.0 -module-name main -emit-ir -o %t/backdeploy.ir
// RUN: %FileCheck %s --check-prefix=BACKDEPLOY < %t/backdeploy.ir
// RUN: %FileCheck %s --check-prefix=BACKDEPLOY-NO-STRONG-REF < %t/backdeploy.ir

// REQUIRES: OS=macosx
// REQUIRES: concurrency

// Make sure that when we introduce `nonsending` overloads of with... functions,
// we don't accidentally break break backdeployment.
//
// Getting this wrong would manifest in runtime crashes and missing symbold when
// running in a backdeployment scenario, so it's worth having this test to verify.

enum TL { static let value = TaskLocal<Int>(wrappedValue: 0) }

@available(macOS 15.0, *)
func testWithUnsafeCurrentTask() async {
  await withUnsafeCurrentTask { _ in
    try? await Task.sleep(nanoseconds: 1)
  }
}

@available(macOS 15.0, *)
func testTaskLocalWithValue() async {
  await TL.value.withValue(1) {
    try? await Task.sleep(nanoseconds: 1)
  }
}

@available(macOS 15.0, *)
func testWithCheckedContinuation() async {
  _ = await withCheckedContinuation { (c: CheckedContinuation<Int, Never>) in
    c.resume(returning: 1)
  }
}

@available(macOS 15.0, *)
func testWithCheckedThrowingContinuation() async throws {
  _ = try await withCheckedThrowingContinuation { (c: CheckedContinuation<Int, Error>) in
    c.resume(returning: 1)
  }
}

@available(macOS 15.0, *)
func testWithUnsafeContinuation() async {
  _ = await withUnsafeContinuation { (c: UnsafeContinuation<Int, Never>) in
    c.resume(returning: 1)
  }
}

@available(macOS 15.0, *)
func testWithUnsafeThrowingContinuation() async throws {
  _ = try await withUnsafeThrowingContinuation { (c: UnsafeContinuation<Int, Error>) in
    c.resume(returning: 1)
  }
}

@available(macOS 15.0, *)
func testWithTaskCancellationHandler() async {
  await withTaskCancellationHandler {
    try? await Task.sleep(nanoseconds: 1)
  } onCancel: { }
}

@available(macOS 15.0, *)
func testTaskGroupWaitForAll() async {
  await withTaskGroup(of: Int.self) { group in
    group.addTask { 1 }
    await group.waitForAll()
  }
}

@available(macOS 15.0, *)
func testThrowingTaskGroupWaitForAll() async throws {
  try await withThrowingTaskGroup(of: Int.self) { group in
    group.addTask { 1 }
    try await group.waitForAll()
  }
}

@available(macOS 15.0, *)
func testThrowingTaskGroupNextResult() async throws {
  try await withThrowingTaskGroup(of: Int.self) { group in
    group.addTask { 1 }
    _ = await group.nextResult()
  }
}

@available(macOS 15.0, *)
func testClockMeasure() async {
  _ = await ContinuousClock().measure {
    try? await Task.sleep(nanoseconds: 1)
  }
}

// In backdeployment, nothing may strongly reference a symbol that only exists in a newer concurrency runtime.
// A `@backDeployed` declaration appears as `declare extern_weak ...`.
//
// BACKDEPLOY-NO-STRONG-REF-NOT: declare swifttailcc {{.*}}Nonsending
// BACKDEPLOY-NO-STRONG-REF-NOT: declare swiftcc {{.*}}Nonsending
// BACKDEPLOY-NO-STRONG-REF: declare extern_weak {{.*}}Nonsending

// withUnsafeCurrentTask could not be made inlinable,
// since UnsafeCurrentTask.init only became usableFromInline in 6.4
// and UnsafeCurrentTask is not @frozen.
//
// It is `@backDeployed` instead, so it must be referenced weakly and carry a fallback definition.
// BACKDEPLOY: define linkonce_odr hidden swifttailcc void @"$ss31withUnsafeCurrentTaskNonsending4bodyxxSctSgYaKYCXE_tYaKlFTwB"
// BACKDEPLOY: declare extern_weak swifttailcc void @"$ss31withUnsafeCurrentTaskNonsending4bodyxxSctSgYaKYCXE_tYaKlF"

// With new enough runtimes, we call the new symbol directly:
// NEW-NO-WEAK-REF-NOT: extern_weak {{.*}}Nonsending
// NEW: declare swifttailcc void @"$ss31withUnsafeCurrentTaskNonsending4bodyxxSctSgYaKYCXE_tYaKlF"
