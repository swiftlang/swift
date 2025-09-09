// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: concurrency_runtime
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: freestanding

func unorderedResults<R>(
    _ fns: [@Sendable () async -> R]) -> (Task<(), Never>, AsyncStream<R>) {
        var capturedContinuation: AsyncStream<R>.Continuation? = nil
        let stream = AsyncStream<R> { continuation in
            capturedContinuation = continuation
        }

        guard let capturedContinuation = capturedContinuation else {
            fatalError("failed to capture continuation")
        }

        let task = Task.detached {
            await withTaskGroup(of: Void.self) { group in
                for fn in fns {
                    group.addTask {
                        let _ = capturedContinuation.yield(await fn())
                    }
                }
                await group.waitForAll()
            }
            capturedContinuation.finish()
        }

        let result = (task, stream)

        return result
    }

var fns: [@Sendable () async -> String] = [
    {
      try? await Task.sleep(nanoseconds: .random(in: 0..<50000))
      return "hello"
    }
]

fns.append(fns[0])
fns.append(fns[0])

// This is a race that will crash or trigger an assertion failure if there's an
// issue. If we get to the end then we pass.
for _ in 0..<1000 {
  let (t, s) = unorderedResults(fns)

  for try await x in s {
    _ = x
    if Bool.random() { t.cancel() }
  }
}
