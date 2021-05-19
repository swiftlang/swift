// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

enum PictureData {
  case value(String)
  case failedToLoadImagePlaceholder
}

@available(SwiftStdlib 5.5, *)
func test_cancellation_checkCancellation() async throws {
  try Task.checkCancellation()
}

@available(SwiftStdlib 5.5, *)
func test_cancellation_guard_isCancelled(_ any: Any) async -> PictureData {
  guard !Task.isCancelled else {
    return PictureData.failedToLoadImagePlaceholder
  }

  return PictureData.value("...")
}

@available(SwiftStdlib 5.5, *)
struct SomeFile: Sendable {
  func close() {}
}

@available(SwiftStdlib 5.5, *)
func test_cancellation_withTaskCancellationHandler(_ anything: Any) async -> PictureData {
  let handle: Task<PictureData, Error> = .init {
    let file = SomeFile()

    return await withTaskCancellationHandler {
      await test_cancellation_guard_isCancelled(file)
    } onCancel: {
      file.close()
    }
  }

  handle.cancel()
}

@available(SwiftStdlib 5.5, *)
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
