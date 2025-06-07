// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -target %target-swift-5.1-abi-triple %import-libdispatch) | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: executable_test

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import Synchronization

struct State {
  var cancelled = 0
  var continuation: CheckedContinuation<Void, Never>?
}

func testFunc(_ iteration: Int) async -> Task<Void, Never> {
  let state = Mutex(State())

  let task = Task {
    await withTaskCancellationHandler {
      await withCheckedContinuation { continuation in
        let cancelled = state.withLock {
          if $0.cancelled > 0 {
            return true
          } else {
            $0.continuation = continuation
            return false
          }
        }
        if cancelled {
          continuation.resume()
        }
      }
    } onCancel: {
      let continuation = state.withLock {
        $0.cancelled += 1
        return $0.continuation.take()
      }
      continuation?.resume()
    }
  }

  // This task cancel is racing with installing the cancellation handler,
  // and we may either hit the cancellation handler:
  // - after this cancel was issued, and therefore the handler runs immediately
  task.cancel()
  _ = await task.value

  let cancelled = state.withLock { $0.cancelled }
  precondition(cancelled == 1, "cancelled more than once, iteration: \(iteration)")

  return task
}

var ts: [Task<Void, Never>] = []
for iteration in 0..<1_000 {
  let t = await testFunc(iteration)
  ts.append(t)
}

print("done") // CHECK: done
