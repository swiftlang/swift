// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

enum PictureData {
  case value(String)
  case failedToLoadImagePlaceholder
}

func test_cancellation_checkCancellation() async throws {
  try Task.checkCancellation()
}

func test_cancellation_guard_isCancelled(_ any: Any) async -> PictureData {
  guard !Task.isCancelled else {
    return PictureData.failedToLoadImagePlaceholder
  }

  return PictureData.value("...")
}

struct SomeFile: Sendable {
  func close() {}
}

func test_cancellation_withCancellationHandler(_ anything: Any) async -> PictureData {
  let handle: Task.Handle<PictureData, Error> = Task.runDetached {
    let file = SomeFile()

    return await Task.withCancellationHandler(
      handler: { file.close() },
      operation: {
      await test_cancellation_guard_isCancelled(file)
    })
  }

  handle.cancel()
}

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
