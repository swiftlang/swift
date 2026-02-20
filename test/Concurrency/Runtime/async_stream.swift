// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking -parse-as-library
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// rdar://78109470
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

import _Concurrency
import StdlibUnittest

struct SomeError: Error, Equatable {
  var value = Int.random(in: 0..<100)
}

class NotSendable {}

@MainActor func testWarnings() {
  var x = 0
  _ = AsyncStream {
    x += 1 // expected-warning {{mutation of captured var 'x' in concurrently-executing code}}
    return 0
  }

  _ = AsyncThrowingStream {
    x += 1 // expected-warning {{mutation of captured var 'x' in concurrently-executing code}}
    return
  }
}

@MainActor var tests = TestSuite("AsyncStream")

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.5, *) {
      final class Expectation: @unchecked Sendable {
        var fulfilled = false
      }

      tests.test("factory method") {
        let (stream, continuation) = AsyncStream.makeStream(of: String.self)
        continuation.yield("hello")

        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
      }

      tests.test("throwing factory method") {
        let (stream, continuation) = AsyncThrowingStream.makeStream(of: String.self, throwing: Error.self)
        continuation.yield("hello")

        var iterator = stream.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with no awaiting next") {
        _ = AsyncStream(String.self) { continuation in
          continuation.yield("hello")
        }
      }

      tests.test("yield with no awaiting next throwing") {
        _ = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
        }
      }

      tests.test("yield with awaiting next") {
        let series = AsyncStream(String.self) { continuation in
          continuation.yield("hello")
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
      }

      tests.test("yield with awaiting next throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2") {
        let series = AsyncStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), "world")
      }

      tests.test("yield with awaiting next 2 throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 and finish") {
        let series = AsyncStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
          continuation.finish()
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), "world")
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("yield with awaiting next 2 and finish throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
          continuation.finish()
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 and throw") {
        let thrownError = SomeError()
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
          continuation.finish(throwing: thrownError)
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          _ = try await iterator.next(isolation: #isolation)
          expectUnreachable("expected thrown error")
        } catch {
          if let failure = error as? SomeError {
            expectEqual(failure, thrownError)
          } else {
            expectUnreachable("unexpected error type")
          }
        }
      }

      tests.test("yield with no awaiting next detached") {
        _ = AsyncStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
          }
        }
      }

      tests.test("yield with no awaiting next detached throwing") {
        _ = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
          }
        }
      }

      tests.test("yield with awaiting next detached") {
        let series = AsyncStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
          }
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
      }

      tests.test("yield with awaiting next detached throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 detached") {
        let series = AsyncStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
          }
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), "world")
      }

      tests.test("yield with awaiting next 2 detached throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 and finish detached") {
        let series = AsyncStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
            continuation.finish()
          }
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), "world")
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("yield with awaiting next 2 and finish detached throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
            continuation.finish()
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 and throw detached") {
        let thrownError = SomeError()
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
            continuation.finish(throwing: thrownError)
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          _ = try await iterator.next(isolation: #isolation)
          expectUnreachable("expected thrown error")
        } catch {
          if let failure = error as? SomeError {
            expectEqual(failure, thrownError)
          } else {
            expectUnreachable("unexpected error type")
          }
        }
      }

      tests.test("yield with awaiting next 2 and finish detached with value after finish") {
        let series = AsyncStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
            continuation.finish()
            continuation.yield("This should not be emitted")
          }
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), "world")
        expectEqual(await iterator.next(isolation: #isolation), nil)
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("yield with awaiting next 2 and finish detached with value after finish throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
            continuation.finish()
            continuation.yield("This should not be emitted")
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 and finish detached with throw after finish throwing") {
        let thrownError = SomeError()
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
            continuation.yield("world")
            continuation.finish()
            continuation.finish(throwing: thrownError)
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with awaiting next 2 and finish with throw after finish throwing") {
        let thrownError = SomeError()
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
          continuation.finish()
          continuation.finish(throwing: thrownError)
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("cancellation behavior on deinit with no values being awaited") {
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          _ = AsyncStream(String.self) { continuation in
            continuation.onTermination = { @Sendable _ in expectation.fulfilled = true }
          }
        }
        
        scopedLifetime(expectation)

        expectTrue(expectation.fulfilled)
      }

      tests.test("termination behavior on deinit with no values being awaited") {
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          _ = AsyncStream(String.self) { continuation in
            continuation.onTermination = { @Sendable _ in expectation.fulfilled = true }
            continuation.finish()
          }
        }
        
        scopedLifetime(expectation)

        expectTrue(expectation.fulfilled)
      }

      tests.test("cancellation behavior on deinit with no values being awaited") {
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          _ = AsyncStream(String.self) { continuation in
            continuation.onTermination = { @Sendable terminal in
              switch terminal {
              case .cancelled:
                expectation.fulfilled = true
              default: break
              }
            }
          }
        }
        
        scopedLifetime(expectation)

        expectTrue(expectation.fulfilled)
      }

      tests.test("cancellation behavior on deinit with no values being awaited throwing") {
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          _ = AsyncThrowingStream(String.self) { continuation in
            continuation.onTermination = { @Sendable terminal in
              switch terminal {
              case .cancelled:
                expectation.fulfilled = true
              default: break
              }
            }
          }
        }
        
        scopedLifetime(expectation)

        expectTrue(expectation.fulfilled)
      }

      tests.test("continuation equality") {
        let (_, continuation1) = AsyncStream<Int>.makeStream()
        let (_, continuation2) = AsyncStream<Int>.makeStream()
        expectTrue(continuation1 == continuation1)
        expectTrue(continuation1 != continuation2)
        expectTrue(continuation1.hashValue == continuation1.hashValue)
        expectTrue(continuation1.hashValue != continuation2.hashValue)
      }

      tests.test("throwing continuation equality") {
        let (_, continuation1) = AsyncThrowingStream<Int, Error>.makeStream()
        let (_, continuation2) = AsyncThrowingStream<Int, Error>.makeStream()
        expectTrue(continuation1 == continuation1)
        expectTrue(continuation1 != continuation2)
        expectTrue(continuation1.hashValue == continuation1.hashValue)
        expectTrue(continuation1.hashValue != continuation2.hashValue)
      }

      tests.test("onTermination receives .finished when finish() is called") {
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          _ = AsyncStream<Int>(Int.self) { continuation in
            continuation.onTermination = { @Sendable terminal in
              if case .finished = terminal {
                expectation.fulfilled = true
              }
            }
            continuation.finish()
          }
        }

        scopedLifetime(expectation)
        expectTrue(expectation.fulfilled)
      }

      // MARK: - YieldResult

      tests.test("yield returns enqueued with remaining count for bounded buffer") {
        let (stream, continuation) = AsyncStream<Int>.makeStream(
          bufferingPolicy: .bufferingNewest(3)
        )

        let r1 = continuation.yield(1)
        let r2 = continuation.yield(2)
        let r3 = continuation.yield(3)

        if case .enqueued(let remaining) = r1 {
          expectEqual(remaining, 2)
        } else {
          expectUnreachable("expected .enqueued, got \(r1)")
        }
        if case .enqueued(let remaining) = r2 {
          expectEqual(remaining, 1)
        } else {
          expectUnreachable("expected .enqueued, got \(r2)")
        }
        if case .enqueued(let remaining) = r3 {
          expectEqual(remaining, 0)
        } else {
          expectUnreachable("expected .enqueued, got \(r3)")
        }

        _ = stream
      }

      tests.test("yield returns dropped when buffer is full") {
        let (stream, continuation) = AsyncStream<Int>.makeStream(
          bufferingPolicy: .bufferingOldest(2)
        )

        _ = continuation.yield(1)
        _ = continuation.yield(2)
        let result = continuation.yield(3)

        if case .dropped(let value) = result {
          expectEqual(value, 3)
        } else {
          expectUnreachable("expected .dropped, got \(result)")
        }

        _ = stream
      }

      tests.test("yield returns terminated after finish") {
        let (_, continuation) = AsyncStream<Int>.makeStream()
        continuation.finish()
        let result = continuation.yield(1)
        if case .terminated = result {
          // expected
        } else {
          expectUnreachable("expected .terminated, got \(result)")
        }
      }

      // MARK: - Multiple consumers

      tests.test("finish behavior with multiple consumers") {
        let (stream, continuation) = AsyncStream<Int>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Int>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        func makeConsumingTaskWithIndex(_ index: Int) -> Task<Void, Never> {
          Task { @MainActor in
            controlContinuation.yield(index)
            for await i in stream {
              controlContinuation.yield(i)
            }
          }
        }

        // Set up multiple consumers
        let consumer1 = makeConsumingTaskWithIndex(1)
        expectEqual(await controlIterator.next(isolation: #isolation), 1)

        let consumer2 = makeConsumingTaskWithIndex(2)
        expectEqual(await controlIterator.next(isolation: #isolation), 2)

        // Ensure the iterators are suspended
        await MainActor.run {}

        // Terminate the stream
        continuation.finish()

        // Ensure the consuming Tasks both complete
        _ = await consumer1.value
        _ = await consumer2.value
      }

      tests.test("element delivery with multiple consumers") {
        final class Collector: @unchecked Sendable {
          var received: [Int] = []
        }
        let collector = Collector()
        let (stream, continuation) = AsyncStream<Int>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Int>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        let consumer1 = Task { @MainActor in
          controlContinuation.yield(1)
          for await value in stream {
            collector.received.append(value)
          }
        }
        expectEqual(await controlIterator.next(isolation: #isolation), 1)

        let consumer2 = Task { @MainActor in
          controlContinuation.yield(2)
          for await value in stream {
            collector.received.append(value)
          }
        }
        expectEqual(await controlIterator.next(isolation: #isolation), 2)

        // Ensure both consumers are suspended in next()
        await MainActor.run {}

        continuation.yield(10)
        continuation.yield(20)
        continuation.yield(30)
        continuation.yield(40)
        continuation.finish()

        _ = await consumer1.value
        _ = await consumer2.value

        // Each element should be delivered to exactly one consumer — none lost or duplicated
        expectEqual(collector.received.sorted(), [10, 20, 30, 40])
      }

      tests.test("finish behavior with multiple consumers throwing") {
        let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Int>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        func makeConsumingTask(_ index: Int) -> Task<Void, Never> {
          Task { @MainActor in
            controlContinuation.yield(index)
            do {
              for try await _ in stream {}
            } catch {
              expectUnreachable("unexpected error: \(error)")
            }
          }
        }

        let consumer1 = makeConsumingTask(1)
        expectEqual(await controlIterator.next(isolation: #isolation), 1)

        let consumer2 = makeConsumingTask(2)
        expectEqual(await controlIterator.next(isolation: #isolation), 2)

        await MainActor.run {}

        continuation.finish()

        _ = await consumer1.value
        _ = await consumer2.value
      }

      tests.test("error delivered to all waiting consumers throwing") {
        let thrownError = SomeError()
        final class Collector: @unchecked Sendable {
          var errorCount = 0
        }
        let collector = Collector()
        let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Int>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        func makeConsumingTask(_ index: Int) -> Task<Void, Never> {
          Task { @MainActor in
            controlContinuation.yield(index)
            do {
              for try await _ in stream {}
            } catch let error as SomeError {
              expectEqual(error, thrownError)
              collector.errorCount += 1
            } catch {
              expectUnreachable("unexpected error type: \(error)")
            }
          }
        }

        let consumer1 = makeConsumingTask(1)
        expectEqual(await controlIterator.next(isolation: #isolation), 1)

        let consumer2 = makeConsumingTask(2)
        expectEqual(await controlIterator.next(isolation: #isolation), 2)

        await MainActor.run {}

        continuation.finish(throwing: thrownError)

        _ = await consumer1.value
        _ = await consumer2.value

        expectEqual(collector.errorCount, 2)
      }

      tests.test("cancellation of one consumer terminates the stream for all consumers") {
        let (stream, _) = AsyncStream<Int>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Int>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        let consumer1 = Task { @MainActor in
          controlContinuation.yield(1)
          for await _ in stream {}
        }
        expectEqual(await controlIterator.next(isolation: #isolation), 1)

        let consumer2 = Task { @MainActor in
          controlContinuation.yield(2)
          for await _ in stream {}
        }
        expectEqual(await controlIterator.next(isolation: #isolation), 2)

        await MainActor.run {}

        // Cancelling consumer1 triggers storage.cancel(), terminating the stream
        // and resuming all waiting continuations — including consumer2's
        consumer1.cancel()

        _ = await consumer1.value
        _ = await consumer2.value
      }

      await runAllTestsAsync()
    }
  }
}


