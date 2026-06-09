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
// XFAIL: OS=emscripten

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

      // MARK: - Buffering Policies

      tests.test("bufferingOldest semantics") {
        typealias Stream = AsyncStream<Int>
        typealias YieldResult = Stream.Continuation.YieldResult
        let (stream, continuation) = Stream.makeStream(
          bufferingPolicy: .bufferingOldest(3)
        )

        let elements = 1...5
        let results = elements.map { continuation.yield($0) }
        continuation.finish()

        let expectedResults = [
          YieldResult.enqueued(remaining: 2),
          .enqueued(remaining: 1),
          .enqueued(remaining: 0),
          .dropped(4),
          .dropped(5),
        ]

        expectEqual(results.count, expectedResults.count)
        for (result, expectedResult) in zip(results, expectedResults) {
          expectTrue(result.isEquivalent(to: expectedResult))
        }

        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), 1)
        expectEqual(await iterator.next(isolation: #isolation), 2)
        expectEqual(await iterator.next(isolation: #isolation), 3)
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("bufferingOldest semantics throwing") {
        typealias Stream = AsyncThrowingStream<Int, Error>
        typealias YieldResult = Stream.Continuation.YieldResult
        let (stream, continuation) = Stream.makeStream(
          bufferingPolicy: .bufferingOldest(2)
        )

        let elements = 1...3
        let results = elements.map { continuation.yield($0) }
        continuation.finish()

        let expectedResults = [
          YieldResult.enqueued(remaining: 1),
          .enqueued(remaining: 0),
          .dropped(3),
        ]

        expectEqual(results.count, expectedResults.count)
        for (result, expectedResult) in zip(results, expectedResults) {
          expectTrue(result.isEquivalent(to: expectedResult))
        }

        do {
          var iterator = stream.makeAsyncIterator()
          expectEqual(try await iterator.next(isolation: #isolation), 1)
          expectEqual(try await iterator.next(isolation: #isolation), 2)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingOldest throwing test")
        }
      }

      tests.test("bufferingNewest semantics") {
        typealias Stream = AsyncStream<Int>
        typealias YieldResult = Stream.Continuation.YieldResult
        let (stream, continuation) = Stream.makeStream(
          bufferingPolicy: .bufferingNewest(3)
        )

        let elements = 1...5
        let results = elements.map { continuation.yield($0) }
        continuation.finish()

        // The dropped value is the evicted oldest, not the incoming element
        let expectedResults = [
          YieldResult.enqueued(remaining: 2),
          .enqueued(remaining: 1),
          .enqueued(remaining: 0),
          .dropped(1),
          .dropped(2),
        ]

        expectEqual(results.count, expectedResults.count)
        for (result, expectedResult) in zip(results, expectedResults) {
          expectTrue(result.isEquivalent(to: expectedResult))
        }

        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), 3)
        expectEqual(await iterator.next(isolation: #isolation), 4)
        expectEqual(await iterator.next(isolation: #isolation), 5)
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("bufferingNewest semantics throwing") {
        typealias Stream = AsyncThrowingStream<Int, Error>
        typealias YieldResult = Stream.Continuation.YieldResult
        let (stream, continuation) = Stream.makeStream(
          bufferingPolicy: .bufferingNewest(2)
        )

        let elements = 1...3
        let results = elements.map { continuation.yield($0) }
        continuation.finish()

        // The dropped value is the evicted oldest, not the incoming element
        let expectedResults = [
          YieldResult.enqueued(remaining: 1),
          .enqueued(remaining: 0),
          .dropped(1),
        ]

        expectEqual(results.count, expectedResults.count)
        for (result, expectedResult) in zip(results, expectedResults) {
          expectTrue(result.isEquivalent(to: expectedResult))
        }

        do {
          var iterator = stream.makeAsyncIterator()
          expectEqual(try await iterator.next(isolation: #isolation), 2)
          expectEqual(try await iterator.next(isolation: #isolation), 3)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingNewest throwing test")
        }
      }

      tests.test("buffering zero capacity drops all") {
        typealias Stream = AsyncStream<Int>
        typealias YieldResult = Stream.Continuation.YieldResult

        // bufferingOldest(0): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingOldest(0)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(await iterator.next(isolation: #isolation), nil)
        }

        // bufferingNewest(0): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingNewest(0)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(await iterator.next(isolation: #isolation), nil)
        }
      }

      tests.test("buffering zero capacity drops all throwing") {
        typealias Stream = AsyncThrowingStream<Int, Error>
        typealias YieldResult = Stream.Continuation.YieldResult

        // bufferingOldest(0): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingOldest(0)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingOldest zero capacity throwing test")
        }

        // bufferingNewest(0): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingNewest(0)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingNewest zero capacity throwing test")
        }
      }

      tests.test("buffering negative capacity drops all") {
        typealias Stream = AsyncStream<Int>
        typealias YieldResult = Stream.Continuation.YieldResult

        // bufferingOldest(-1): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingOldest(-1)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(await iterator.next(isolation: #isolation), nil)
        }

        // bufferingNewest(-1): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingNewest(-1)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(await iterator.next(isolation: #isolation), nil)
        }
      }

      tests.test("buffering negative capacity drops all throwing") {
        typealias Stream = AsyncThrowingStream<Int, Error>
        typealias YieldResult = Stream.Continuation.YieldResult

        // bufferingOldest(-1): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingOldest(-1)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingOldest negative capacity throwing test")
        }

        // bufferingNewest(-1): every yield dropped immediately
        do {
          let (stream, continuation) = Stream.makeStream(
            bufferingPolicy: .bufferingNewest(-1)
          )
          let results = (1...2).map { continuation.yield($0) }
          continuation.finish()

          let expectedResults = [YieldResult.dropped(1), .dropped(2)]
          expectEqual(results.count, expectedResults.count)
          for (result, expectedResult) in zip(results, expectedResults) {
            expectTrue(result.isEquivalent(to: expectedResult))
          }

          var iterator = stream.makeAsyncIterator()
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingNewest negative capacity throwing test")
        }
      }

      // MARK: - YieldResult Inspection

      tests.test("yield result unbounded remaining") {
        _ = AsyncStream<String> { continuation in
          expectTrue(continuation.yield("hello").isEquivalent(to: .enqueued(remaining: Int.max)))
        }
      }

      tests.test("yield result unbounded remaining throwing") {
        _ = AsyncThrowingStream<String, Error> { continuation in
          expectTrue(continuation.yield("hello").isEquivalent(to: .enqueued(remaining: Int.max)))
        }
      }

      tests.test("yield result terminated") {
        _ = AsyncStream<String> { continuation in
          continuation.finish()
          expectTrue(continuation.yield("after finish").isEquivalent(to: .terminated))
        }
      }

      tests.test("yield result terminated throwing") {
        _ = AsyncThrowingStream<String, Error> { continuation in
          continuation.finish(throwing: SomeError())
          expectTrue(continuation.yield("after throw").isEquivalent(to: .terminated))
        }
      }

      // MARK: - Alternative Yield Methods

      tests.test("yield with success result") {
        let (stream, continuation) = AsyncStream<String>.makeStream()
        continuation.yield(with: .success("hello"))
        continuation.finish()
        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("yield with success result throwing") {
        let (stream, continuation) = AsyncThrowingStream<String, Error>.makeStream()
        continuation.yield(with: .success("world"))
        continuation.finish()
        var iterator = stream.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("yield with failure result terminates stream throwing") {
        let thrownError = SomeError()
        let series = AsyncThrowingStream<String, Error> { continuation in
          continuation.yield("before error")
          let result = continuation.yield(with: .failure(thrownError))
          if case .terminated = result { }
          else { expectUnreachable("expected .terminated from yield(with: .failure)") }
          // yields after error are also .terminated
          let afterError = continuation.yield("should not appear")
          if case .terminated = afterError { }
          else { expectUnreachable("expected .terminated after error") }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "before error")
          _ = try await iterator.next(isolation: #isolation)
          expectUnreachable("expected thrown error")
        } catch {
          if let failure = error as? SomeError {
            expectEqual(failure, thrownError)
          } else {
            expectUnreachable("unexpected error type")
          }
        }
        // After the error is consumed, further next() calls return nil
        do {
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("expected nil after error was consumed, not another throw")
        }
      }

      tests.test("yield void element") {
        let (stream, continuation) = AsyncStream<Void>.makeStream()
        let result = continuation.yield()
        continuation.finish()

        if case .enqueued = result { }
        else { expectUnreachable("expected .enqueued for void yield") }

        var iterator = stream.makeAsyncIterator()
        let value: Void? = await iterator.next(isolation: #isolation)
        expectNotNil(value)
        expectNil(await iterator.next(isolation: #isolation))
      }

      tests.test("yield void element throwing") {
        let (stream, continuation) = AsyncThrowingStream<Void, Error>.makeStream()
        let result = continuation.yield()
        continuation.finish()

        if case .enqueued = result { }
        else { expectUnreachable("expected .enqueued for void yield") }

        var iterator = stream.makeAsyncIterator()
        do {
          let value: Void? = try await iterator.next(isolation: #isolation)
          expectNotNil(value)
          expectNil(try await iterator.next(isolation: #isolation))
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      // MARK: - Unfolding Initializer

      tests.test("unfolding basic") {
        nonisolated(unsafe) var counter = 0
        let stream = AsyncStream<Int>(unfolding: {
          counter += 1
          return counter <= 3 ? counter : nil
        })
        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), 1)
        expectEqual(await iterator.next(isolation: #isolation), 2)
        expectEqual(await iterator.next(isolation: #isolation), 3)
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("unfolding init throwing") {
        nonisolated(unsafe) var counter = 0
        let stream = AsyncThrowingStream<Int, Error>(unfolding: {
          counter += 1
          return counter <= 3 ? counter : nil
        })
        var iterator = stream.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), 1)
          expectEqual(try await iterator.next(isolation: #isolation), 2)
          expectEqual(try await iterator.next(isolation: #isolation), 3)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("unfolding init onCancel called on task cancel") {
        let expectation = Expectation()

        var counter = 0
        let stream = AsyncStream<Int>(unfolding: { @MainActor in
          counter += 1
          if counter == 2 {
            withUnsafeCurrentTask { $0?.cancel() }
          }
          return counter
        }, onCancel: { @Sendable in
          expectation.fulfilled = true
        })

        let task = Task { @MainActor in
          var it = stream.makeAsyncIterator()
          let one = await it.next(isolation: #isolation)
          expectEqual(one, 1)

          let two = await it.next(isolation: #isolation)
          expectTrue(expectation.fulfilled)
          expectEqual(two, 2)

          // Now we should be terminated
          let three = await it.next(isolation: #isolation)
          expectNil(three)
        }

        await task.value
      }

      tests.test("unfolding init throwing throws from closure") {
        var counter = 0
        let thrownError = SomeError()
        let stream = AsyncThrowingStream<Int, Error>(unfolding: { @MainActor in
          counter += 1
          if counter == 3 { throw thrownError }
          return counter < 3 ? counter : nil
        })
        var iterator = stream.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), 1)
          expectEqual(try await iterator.next(isolation: #isolation), 2)
          _ = try await iterator.next(isolation: #isolation)
          expectUnreachable("expected thrown error on third call")
        } catch {
          if let failure = error as? SomeError {
            expectEqual(failure, thrownError)
          } else {
            expectUnreachable("unexpected error type")
          }
        }
        // After the error, subsequent next() calls return nil
        do {
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("expected nil after error, not another throw")
        }
      }

      // MARK: - onTermination

      tests.test("onTermination finished reason") {
        let expectation = Expectation()
        let _ = AsyncStream<String> {
          $0.onTermination = { @Sendable reason in
            if case .finished = reason {
              expectation.fulfilled = true
            }
          }
          $0.finish()
        }
        expectTrue(expectation.fulfilled)
      }

      tests.test("onTermination finished reason throwing") {
        // Case A: finish() with no error -> .finished(nil)
        let nilExpectation = Expectation()
        let _ = AsyncThrowingStream<String, Error> {
          $0.onTermination = { @Sendable reason in
            if case .finished(let e) = reason, e == nil {
              nilExpectation.fulfilled = true
            }
          }
          $0.finish()
        }
        expectTrue(nilExpectation.fulfilled)

        // Case B: finish(throwing:) -> .finished(error)
        let errExpectation = Expectation()
        let thrownError = SomeError()
        let _ = AsyncThrowingStream<String, Error> {
          $0.onTermination = { @Sendable reason in
            if case .finished(let e) = reason,
               let failure = e as? SomeError,
               failure == thrownError {
              errExpectation.fulfilled = true
            }
          }
          $0.finish(throwing: thrownError)
        }
        expectTrue(errExpectation.fulfilled)
      }

      tests.test("onTermination called once") {
        nonisolated(unsafe) var counter = 0
        let (_, continuation) = AsyncStream<String>.makeStream()
        continuation.onTermination = { @Sendable _ in counter += 1 }
        continuation.finish()
        continuation.finish() // handler should be cleared
        expectEqual(counter, 1)
      }

      tests.test("onTermination called once throwing") {
        nonisolated(unsafe) var counter = 0
        let (_, continuation) = AsyncThrowingStream<String, Error>.makeStream()
        continuation.onTermination = { @Sendable _ in counter += 1 }
        continuation.finish()
        continuation.finish() // handler should be cleared
        expectEqual(counter, 1)
      }

      // MARK: - for try await

      tests.test("for try await") {
        // Normal completion: all yielded values collected
        do {
          let (stream, continuation) = AsyncThrowingStream<String, Error>.makeStream()
          continuation.yield("a")
          continuation.yield("b")
          continuation.finish()
          expectTrue(continuation.yield("c").isEquivalent(to: .terminated))
          var collected: [String] = []
          do {
            for try await value in stream { collected.append(value) }
          } catch {
            expectUnreachable("unexpected error in normal completion")
          }
          expectEqual(collected, ["a", "b"])
        }

        // Error propagation: values before throw are collected; error is caught
        do {
          let (stream, continuation) = AsyncThrowingStream<String, Error>.makeStream()
          let thrownError = SomeError()
          continuation.yield("x")
          continuation.yield("y")
          continuation.finish(throwing: thrownError)
          expectTrue(continuation.yield("c").isEquivalent(to: .terminated))
          var collected: [String] = []
          do {
            for try await value in stream { collected.append(value) }
            expectUnreachable("expected thrown error")
          } catch {
            if let failure = error as? SomeError {
              expectEqual(failure, thrownError)
            } else {
              expectUnreachable("unexpected error type")
            }
          }
          expectEqual(collected, ["x", "y"])
        }
      }

      // MARK: - Parity

      tests.test("onTermination after finish") {
        let series = AsyncStream(String.self) { continuation in
          nonisolated(unsafe) var terminalCallCount = 0
          continuation.onTermination = { @Sendable _ in terminalCallCount += 1 }
          continuation.yield("hello")
          continuation.finish()
          expectEqual(terminalCallCount, 1)
          continuation.finish() // second finish is a no-op
          expectEqual(terminalCallCount, 1)
        }
        var iterator = series.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), nil)
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("onTermination after finish throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          nonisolated(unsafe) var terminalCallCount = 0
          continuation.onTermination = { @Sendable _ in terminalCallCount += 1 }
          continuation.yield("hello")
          continuation.finish()
          expectEqual(terminalCallCount, 1)
          continuation.finish() // second finish is a no-op
          expectEqual(terminalCallCount, 1)
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      // MARK: - Task Cancellation

      // FIXME: https://github.com/swiftlang/swift/issues/88366
      // These tests were flaking indeterminately on CI and the root
      // cause has yet to be determined.
      /*
      tests.test("task cancellation terminates stream") {
        let expectation = Expectation()
        let (stream, continuation) = AsyncStream<Int>.makeStream()
        continuation.onTermination = { @Sendable reason in
          if case .cancelled = reason { expectation.fulfilled = true }
        }
        let (controlStream, controlContinuation) = AsyncStream<Void>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        let task = Task { @MainActor in
          var iterator = stream.makeAsyncIterator()
          controlContinuation.yield(())
          return await iterator.next(isolation: #isolation)
        }

        _ = await controlIterator.next(isolation: #isolation)
        await MainActor.run {}
        expectNotNil(continuation.onTermination)
        task.cancel()
        expectNil(continuation.onTermination) // handler should be cleared on cancel
        let result = await task.value
        expectEqual(result, nil)
        expectTrue(expectation.fulfilled)
      }

      tests.test("task cancellation terminates throwing stream") {
        let (stream, _) = AsyncThrowingStream<Int, Error>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Void>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        let task = Task { @MainActor in
          var iter = stream.makeAsyncIterator()
          controlContinuation.yield(())
          return try await iter.next(isolation: #isolation)
        }

        _ = await controlIterator.next(isolation: #isolation)
        await MainActor.run {}
        task.cancel()

        do {
          let result = try await task.value
          expectEqual(result, nil)
        } catch {
          expectUnreachable("should not throw")
        }
      }
       */

      await runAllTestsAsync()
    }
  }
}

// MARK: - Utilities

fileprivate extension AsyncStream.Continuation.YieldResult where Element: Equatable {
  func isEquivalent(to other: Self) -> Bool {
    switch (self, other) {
    case (.enqueued(let lhs), .enqueued(let rhs)): lhs == rhs
    case (.dropped(let lhs), .dropped(let rhs)): lhs == rhs
    case (.terminated, .terminated): true
    default: false
    }
  }
}

fileprivate extension AsyncThrowingStream.Continuation.YieldResult where Element: Equatable {
  func isEquivalent(to other: Self) -> Bool {
    switch (self, other) {
    case (.enqueued(let lhs), .enqueued(let rhs)): lhs == rhs
    case (.dropped(let lhs), .dropped(let rhs)): lhs == rhs
    case (.terminated, .terminated): true
    default: false
    }
  }
}
