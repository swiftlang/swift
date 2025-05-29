// RUN: %target-run-simple-swift( -target %target-swift-5.1-abi-triple %import-libdispatch)
// REQUIRES: concurrency
// REQUIRES: executable_test

// REQUIRES: concurrency_runtime
// UNSUPPORTED: freestanding

func recurseABunch(_ call: () async throws -> Void, n: Int = 100) async throws {
  try await withTaskCancellationHandler {
    if n == 0 {
      try await call()
      return
    }

    try await recurseABunch(call, n: n - 1)
  } onCancel: {
  }
}

for _ in 0..<100_000 {
    let task: Task<Void, any Error> = Task {
        try await Task.sleep(nanoseconds: UInt64.random(in: 0..<1_000))
        try await recurseABunch() {
          await withTaskCancellationHandler {
          } onCancel: {
          }
        }
    }
    Task {
        try await Task.sleep(nanoseconds: UInt64.random(in: 0..<1_000))
        task.cancel()
    }
}
