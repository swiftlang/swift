// RUN: %target-run-simple-swift( -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: synchronization

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding
// XFAIL: OS=emscripten

// rdar://156074753 / https://github.com/swiftlang/swift/issues/83137
// AsyncStream(unfolding:onCancel:) must call onCancel at most once.

import _Concurrency
import StdlibUnittest
import Synchronization

@available(SwiftStdlib 6.2, *)
final class Gate: Sendable {
  private let continuation = Mutex<CheckedContinuation<Void, Never>?>(nil)

  /// Suspends until `open()` is called, without observing cancellation.
  /// `onSuspended` runs once the continuation is installed, so `open()` can
  /// no longer be missed.
  func wait(onSuspended: () -> Void) async {
    await withCheckedContinuation { c in
      continuation.withLock { $0 = c }
      onSuspended()
    }
  }

  func open() {
    continuation.withLock { $0.take() }!.resume()
  }
}

@MainActor var tests = TestSuite("AsyncStreamUnfoldingCancel")

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 6.2, *) {

      tests.test("onCancel once when producer ignores cancellation") {
        let cancelCount = Mutex(0)
        let produceCount = Mutex(0)
        let gate = Gate()
        let (suspended, suspendedContinuation) = AsyncStream.makeStream(of: Void.self)

        let stream = AsyncStream<Int>(unfolding: {
          let n = produceCount.withLock { $0 += 1; return $0 }
          // Ignore cancellation and produce a value regardless.
          await gate.wait { suspendedContinuation.yield() }
          return n
        }, onCancel: {
          cancelCount.withLock { $0 += 1 }
        })

        let task = Task {
          var values: [Int] = []
          for await value in stream { values.append(value) }
          return values
        }

        // Wait until the first next() is suspended inside the producer.
        for await _ in suspended { break }
        task.cancel()
        expectEqual(cancelCount.withLock { $0 }, 1)
        gate.open()

        // The in-flight element is still delivered, then the stream ends.
        let values = await task.value
        expectEqual(values, [1])
        expectEqual(produceCount.withLock { $0 }, 1)
        expectEqual(cancelCount.withLock { $0 }, 1)
      }

      tests.test("onCancel once when cancelled during suspended next()") {
        let cancelCount = Mutex(0)
        let (suspended, suspendedContinuation) = AsyncStream.makeStream(of: Void.self)

        let stream = AsyncStream<Int>(unfolding: {
          // Honour cancellation: suspend until cancelled, then finish.
          suspendedContinuation.yield()
          try? await Task.sleep(for: .seconds(3600))
          return Task.isCancelled ? nil : 1
        }, onCancel: {
          cancelCount.withLock { $0 += 1 }
        })

        let task = Task {
          var values: [Int] = []
          for await value in stream { values.append(value) }
          // Further next() calls after termination must not re-fire onCancel.
          var iterator = stream.makeAsyncIterator()
          expectNil(await iterator.next())
          return values
        }

        for await _ in suspended { break }
        task.cancel()

        let values = await task.value
        expectEqual(values, [])
        expectEqual(cancelCount.withLock { $0 }, 1)
      }

      tests.test("onCancel not called when cancelled after normal finish") {
        let cancelCount = Mutex(0)
        let produceCount = Mutex(0)

        let stream = AsyncStream<Int>(unfolding: {
          produceCount.withLock { $0 += 1; return $0 <= 2 ? $0 : nil }
        }, onCancel: {
          cancelCount.withLock { $0 += 1 }
        })

        let task = Task {
          var iterator = stream.makeAsyncIterator()
          expectEqual(await iterator.next(), 1)
          expectEqual(await iterator.next(), 2)
          expectNil(await iterator.next())
          withUnsafeCurrentTask { $0?.cancel() }
          expectNil(await iterator.next())
          expectNil(await iterator.next())
        }

        await task.value
        expectEqual(produceCount.withLock { $0 }, 3)
        expectEqual(cancelCount.withLock { $0 }, 0)
      }
    }

    await runAllTestsAsync()
  }
}
