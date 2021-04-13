// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

enum PictureData {
  case value(String)
  case failedToLoadImagePlaceholder
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_cancellation_checkCancellation() async throws {
  try Task.checkCancellation()
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_cancellation_guard_isCancelled(_ any: Any) async -> PictureData {
  guard !Task.isCancelled else {
    return PictureData.failedToLoadImagePlaceholder
  }

  return PictureData.value("...")
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
struct SomeFile: Sendable {
  func close() {}
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
func test_cancellation_withTaskCancellationHandler(_ anything: Any) async -> PictureData {
  let handle: Task.Handle<PictureData, Error> = detach {
    let file = SomeFile()

    return await withTaskCancellationHandler(
      handler: { file.close() }) {
      await test_cancellation_guard_isCancelled(file)
    }
  }

  handle.cancel()
}

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
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
