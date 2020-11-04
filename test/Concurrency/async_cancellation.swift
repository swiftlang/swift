// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

enum PictureData {
  case value(String)
  case failedToLoadImagePlaceholder
}

func test_cancellation_checkCancellation() async throws {
  await try Task.checkCancellation()
}

func test_cancellation_guard_isCancelled(_ any: Any) async -> PictureData {
  guard await !Task.isCancelled() else {
    return PictureData.failedToLoadImagePlaceholder
  }

  return PictureData.value("...")
}

struct SomeFile {
  func close() {}
}

func test_cancellation_withCancellationHandler(_ anything: Any) async -> PictureData {
  let handle = Task.runDetached { () -> PictureData in
    let file = SomeFile()

    return await try Task.withCancellationHandler(
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
  for t in tasks where await !Task.isCancelled() {
    await t.process()
    processed += 1
  }
  return processed
}

// ==== Deadlines --------------------------------------------------------------

func int() async -> Int { 42 }

func test_cancellation_withDeadline_in() async throws -> Int {
  await Task.withDeadline(in: .seconds(5), operation: {
    await int()
  })
}

func test_cancellation_withDeadline(specificDeadline: Task.Deadline) async -> Int {
  await Task.withDeadline(specificDeadline) {
    await int()
  }
}
