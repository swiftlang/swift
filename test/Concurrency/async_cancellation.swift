// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency
// REQUIRES: asserts

enum PictureData {
  case value(String)
  case failedToLoadImagePlaceholder
}

@available(SwiftStdlib 5.1, *)
func test_cancellation_checkCancellation() async throws {
  try Task.checkCancellation()
}

@available(SwiftStdlib 5.1, *)
func test_cancellation_guard_isCancelled(_ any: Any) async -> PictureData {
  guard !Task.isCancelled else {
    return PictureData.failedToLoadImagePlaceholder
  }

  return PictureData.value("...")
}

@available(SwiftStdlib 5.1, *)
struct SomeFile: Sendable {
  func close() {}
}

enum HomeworkError: Error {
case dogAteIt
}

@available(SwiftStdlib 5.1, *)
func test_cancellation_withTaskCancellationHandler(_ anything: Any) async -> PictureData? {
  let handle: Task<PictureData, Error> = .init {
    let file = SomeFile()

    do throws(HomeworkError) {
      return try await withTaskCancellationHandler { () throws(HomeworkError) in
        await test_cancellation_guard_isCancelled(file)
      } onCancel: {
        file.close()
      }
    } catch .dogAteIt {
      return PictureData.value("...")
    }
  }

  handle.cancel()
  return nil
}

@available(SwiftStdlib 6.0, *)
func test_cancellation_withTaskCancellationHandler(_ anything: Any, isolation: (any Actor)? = #isolation) async -> PictureData? {
  let handle: Task<PictureData, Error> = .init {
    let file = SomeFile()

    do {
      return try await withTaskCancellationHandler(
        operation: { () throws in
          await test_cancellation_guard_isCancelled(file)
        },
        onCancel: {
          file.close()
        },
        isolation: isolation
      )
    } catch {
      return PictureData.value("...")
    }
  }

  handle.cancel()
  return nil
}

// Synchronous code uses the synchronous overloads
@available(SwiftStdlib 6.5, *)
func test_cancellation_withTaskCancellationHandler_sync() throws(HomeworkError) -> Int {
  let file = SomeFile()
  return try withTaskCancellationHandler { () throws(HomeworkError) in
    42
  } onCancel: {
    file.close()
  }
}

@available(SwiftStdlib 6.5, *)
func test_cancellation_withTaskCancellationHandler_sync_reason() -> Int {
  withTaskCancellationHandler {
    42
  } onCancel: { reason in
    _ = reason
  }
}

@available(SwiftStdlib 6.5, *)
func test_withTaskPriorityEscalationHandler_sync() -> Int {
  withTaskPriorityEscalationHandler {
    42
  } onPriorityEscalated: { _, _ in
  }
}

@available(SwiftStdlib 6.5, *)
func test_cancellation_withTaskCancellationHandler_syncOperation() async -> Int {
  let a = await withTaskCancellationHandler { // expected-warning@:11{{no 'async' operations occur within 'await' expression}}{{11-17=}}
    42
  } onCancel: {
  }
  let b = await withTaskCancellationHandler { // expected-warning@:11{{no 'async' operations occur within 'await' expression}}{{11-17=}}
    42
  } onCancel: { reason in
    _ = reason
  }
  let c = await withTaskPriorityEscalationHandler { // expected-warning@:11{{no 'async' operations occur within 'await' expression}}{{11-17=}}
    42
  } onPriorityEscalated: { _, _ in
  }
  return a + b + c
}

@available(SwiftStdlib 5.1, *)
func test_cancellation_loop() async -> Int {
  struct SampleTask { func process() async {} }

  let tasks = [SampleTask(), SampleTask()]
  var processed = 0
  for t in tasks where !Task.isCancelled {
    await t.process()
    processed += 1
  }
  return processed
}
