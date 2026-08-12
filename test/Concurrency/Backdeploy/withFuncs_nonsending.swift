// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx27.0 -module-name main -emit-ir -o %t/new.ir
// RUN: %FileCheck %s --check-prefix=NEW < %t/new.ir
// RUN: %FileCheck %s --check-prefix=NEW-NO-EXTERNAL-REF < %t/new.ir

// RUN: %target-swift-frontend %s -target %target-cpu-apple-macosx15.0 -module-name main -emit-ir -o %t/backdeploy.ir
// RUN: %FileCheck %s --check-prefix=BACKDEPLOY < %t/backdeploy.ir
// RUN: %FileCheck %s --check-prefix=BACKDEPLOY-NO-EXTERNAL-REF < %t/backdeploy.ir

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

// In backdeployment, nothing may reference a symbol that only exists in a newer concurrency runtime.
//
// BACKDEPLOY-NO-EXTERNAL-REF-NOT: declare {{.*}}Nonsending
// NEW-NO-EXTERNAL-REF-NOT: declare {{.*}}Nonsending

// BACKDEPLOY: define linkonce_odr hidden swifttailcc void @"$ss43withUnsafeCurrentTaskNonsendingExportedImpl4bodyxxSctSgYaKYCXE_tYaKlF"
// NEW: define linkonce_odr hidden swifttailcc void @"$ss43withUnsafeCurrentTaskNonsendingExportedImpl4bodyxxSctSgYaKYCXE_tYaKlF"
