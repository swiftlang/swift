// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking -parse-as-library
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)

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
    x += 1 // expected-warning {{mutation of captured var 'x' in concurrently-executing code; this is an error in the Swift 6 language mode}}
    return 0
  }

  _ = AsyncThrowingStream {
    x += 1 // expected-warning {{mutation of captured var 'x' in concurrently-executing code; this is an error in the Swift 6 language mode}}
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

      // MARK: - Multiple consumers

      tests.test("FIFO delivery order with multiple consumers") {
        let (stream, continuation) = AsyncStream<Int>.makeStream()
        let (observerStream, observerContinuation) = AsyncStream<String>.makeStream()
        var observerIterator = observerStream.makeAsyncIterator()

        // Set up multiple consumers
        let consumers = await makeConsumingTasks(
          for: stream,
          count: 3,
          observerStreamIterator: &observerIterator,
          observerContinuation: observerContinuation
        )

        // Yield the elements & complete
        continuation.yield(1)
        continuation.yield(2)
        continuation.yield(3)
        continuation.finish()

        // Results may be propagated through the observer stream in an arbitrary order,
        // but the continuation resumption order from the stream under test should be
        // correctly encoded in the string values, so we use a Set here.
        var results: Set<String> = []
        for _ in consumers {
          let next = await observerIterator.next(isolation: #isolation)!
          results.insert(next)
        }

        expectEqual(results, [
          "consumer 1 got: 1",
          "consumer 2 got: 2",
          "consumer 3 got: 3",
        ])

        // Ensure the consuming Tasks complete
        for consumer in consumers {
          _ = await consumer.value
        }
      }

      tests.test("FIFO delivery order with multiple consumers throwing") {
        let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
        let (observerStream, observerContinuation) = AsyncStream<String>.makeStream()
        var observerIterator = observerStream.makeAsyncIterator()

        // Set up multiple consumers
        let consumers = await makeConsumingTasks(
          for: stream,
          count: 4,
          observerStreamIterator: &observerIterator,
          observerContinuation: observerContinuation,
          // have each consuming task only listen for one stream element to avoid
          // trying to micro-manage races b/w normal element consumption and
          // termination with an error
          shouldEndConsumptionUponElementReceipt: true
        )

        // Emit some elements then terminate by throwing
        continuation.yield(1)
        continuation.yield(2)
        continuation.finish(throwing: SomeError(value: 42))

        var results: Set<String> = []
        for _ in consumers {
          let next = await observerIterator.next(isolation: #isolation)!
          results.insert(next)
        }

        expectEqual(results, [
          "consumer 1 got: 1",
          "consumer 2 got: 2",
          "consumer 3 got error: 42",
          "consumer 4 got error: 42",
        ])

        // Ensure the consuming Tasks complete
        for consumer in consumers {
          _ = await consumer.value
        }
      }

      tests.test("finish behavior with multiple consumers") {
        let (stream, continuation) = AsyncStream<Int>.makeStream()
        let (observerStream, observerContinuation) = AsyncStream<String>.makeStream()
        var observerIterator = observerStream.makeAsyncIterator()

        // Set up multiple consumers
        let consumers = await makeConsumingTasks(
          for: stream,
          count: 2,
          observerStreamIterator: &observerIterator,
          observerContinuation: observerContinuation
        )

        // Terminate the stream
        continuation.finish()

        // Ensure the consuming Tasks both complete
        for consumer in consumers {
          _ = await consumer.value
        }
      }

      tests.test("finish behavior with multiple consumers throwing error") {
        let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
        let (observerStream, observerContinuation) = AsyncStream<String>.makeStream()
        var observerIterator = observerStream.makeAsyncIterator()

        // Set up multiple consumers
        let consumers = await makeConsumingTasks(
          for: stream,
          count: 2,
          observerStreamIterator: &observerIterator,
          observerContinuation: observerContinuation
        )

        // Terminate the stream with an error
        continuation.finish(throwing: SomeError(value: 42))

        var results: [String] = []
        for _ in consumers {
          let next = await observerIterator.next(isolation: #isolation)
          results.append(next!)
        }

        expectEqual(Set(results), [
          "consumer 1 got error: 42",
          "consumer 2 got error: 42",
        ])

        // Ensure the consuming Tasks both complete
        for consumer in consumers {
          _ = await consumer.value
        }
      }

      tests.test("finish behavior with multiple consumers throwing no error") {
        let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
        let (observerStream, observerContinuation) = AsyncStream<String>.makeStream()
        var observerIterator = observerStream.makeAsyncIterator()

        // Set up multiple consumers
        let consumers = await makeConsumingTasks(
          for: stream,
          count: 2,
          observerStreamIterator: &observerIterator,
          observerContinuation: observerContinuation
        )

        // Terminate the stream
        continuation.finish()

        // Ensure the consuming Tasks both complete
        for consumer in consumers {
          _ = await consumer.value
        }
      }

      await runAllTestsAsync()
    }
  }
}

// MARK: - Utilities

@globalActor
actor EvenActor {
  static let shared = EvenActor()
  static func run() async { await Task { @EvenActor in () }.value }
}

@globalActor
actor OddActor {
  static let shared = OddActor()
  static func run() async { await Task { @OddActor in () }.value }
}

/// Sets up a consuming task with an observer stream that reports on its progress
private func makeConsumingTaskWithIndex<S: AsyncSequence>(
  _ index: Int,
  sequence: S,
  observerContinuation: AsyncStream<String>.Continuation,
  shouldEndConsumptionUponElementReceipt: Bool = false
) -> Task<Void, Never> 
where S.Element == Int
{
  let task: Task<Void, Never>
  // Task bodies here are duplicated & inlined to ensure the only
  // potential suspension point is the iteration of the sequence
  if index % 2 == 0 {
    task = Task { @EvenActor in
      observerContinuation.yield("started task: \(index)")
      do {
        for try await i in sequence {
          observerContinuation.yield("consumer \(index) got: \(i)")
          if shouldEndConsumptionUponElementReceipt { break }
        }
      } catch let error as SomeError {
        observerContinuation.yield("consumer \(index) got error: \(error.value)")
      } catch {
        expectUnreachable("caught unexpected error")
      }
    }
  } else {
    task = Task { @OddActor in
      observerContinuation.yield("started task: \(index)")
      do {
        for try await i in sequence {
          observerContinuation.yield("consumer \(index) got: \(i)")
          if shouldEndConsumptionUponElementReceipt { break }
        }
      } catch let error as SomeError {
        observerContinuation.yield("consumer \(index) got error: \(error.value)")
      } catch {
        expectUnreachable("caught unexpected error")
      }
    }
  }

  return task
}

/// Sets up `count` consuming tasks. The Task index's parity determines which global
/// actor it will be isolated to.
private func makeConsumingTasks<S: AsyncSequence>(
  for sequence: S,
  count: Int,
  observerStreamIterator: inout AsyncStream<String>.Iterator,
  observerContinuation: AsyncStream<String>.Continuation,
  shouldEndConsumptionUponElementReceipt: Bool = false
) async -> [Task<Void, Never>]
where S.Element == Int
{
  var results: [Task<Void, Never>] = []
  for i in 1...count {
    let task = makeConsumingTaskWithIndex(
      i,
      sequence: sequence,
      observerContinuation: observerContinuation,
      shouldEndConsumptionUponElementReceipt: shouldEndConsumptionUponElementReceipt
    )

    // Wait for the Task to start
    expectEqual(await observerStreamIterator.next(isolation: #isolation), "started task: \(i)")

    results.append(task)
  }

  // Ensure the iterators are suspended
  await EvenActor.run()
  await OddActor.run()

  return results
}
