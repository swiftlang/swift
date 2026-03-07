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

      tests.test("unfold with no awaiting next throwing") {
        _ = AsyncThrowingStream(unfolding: { return "hello" })
      }

      tests.test("unfold with no awaiting next uncancelled throwing") {
        _ = AsyncThrowingStream(
          unfolding: { return "hello" },
          onCancel: { expectUnreachable("unexpected cancellation") }
        )
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

      tests.test("unfold with awaiting next throwing") {
        let series = AsyncThrowingStream(unfolding: { return "hello" })
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }

      tests.test("unfold with awaiting next uncancelled throwing") {
        let series = AsyncThrowingStream(
          unfolding: { return "hello" },
          onCancel: { expectUnreachable("unexpected cancellation") }
        )
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

      tests.test("unfold with awaiting next 2 throwing") {
        var values = ["hello", "world"].makeIterator()
        let series = AsyncThrowingStream(
          unfolding: { @MainActor in return values.next() }
        )
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }
      
      tests.test("unfold with awaiting next 2 uncancelled throwing") {
        var values = ["hello", "world"].makeIterator()
        let series = AsyncThrowingStream(
          unfolding: { @MainActor in return values.next() },
          onCancel: { expectUnreachable("unexpected cancellation") }
        )
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

      tests.test("unfold with awaiting next 2 and finish throwing") {
        var values = ["hello", "world"].makeIterator()
        let series = AsyncThrowingStream(
          unfolding: {@MainActor in return values.next() }
        )
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(isolation: #isolation), "hello")
          expectEqual(try await iterator.next(isolation: #isolation), "world")
          expectEqual(try await iterator.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error thrown")
        }
      }
      
      tests.test("unfold awaiting next 2 and finish uncancelled throwing") {
        var values = ["hello", "world"].makeIterator()
        let series = AsyncThrowingStream(
          unfolding: { @MainActor in return values.next() },
          onCancel: { expectUnreachable("unexpected cancellation") }
        )
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

      tests.test("unfold with awaiting next 2 and throw") {
        let thrownError = SomeError()
        var values = ["hello", "world"].makeIterator()
        let series = AsyncThrowingStream(
          unfolding: { @MainActor in
            guard let value = values.next() else { throw thrownError }
            return value
          }
        )
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
      
      tests.test("unfold awaiting next 2 and finish uncancelled and throw") {
        let thrownError = SomeError()
        var values = ["hello", "world"].makeIterator()
        let series = AsyncThrowingStream(
          unfolding: { @MainActor in
            guard let value = values.next() else { throw thrownError }
            return value
          }, onCancel: { expectUnreachable("unexpected cancellation") }
        )
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

      tests.test("unfold awaiting next and cancel throwing") {
        let expectation = Expectation()
        let task = Task.detached {
          let series = AsyncThrowingStream(
            unfolding: {
              withUnsafeCurrentTask { $0?.cancel() }
              return "hello"
            }, onCancel: { expectation.fulfilled = true }
          )
          var iterator = series.makeAsyncIterator()
          do {
            expectEqual(try await iterator.next(isolation: #isolation), "hello")
          } catch {
            expectUnreachable("unexpected error thrown")
          }
        }
        _ = await task.getResult()
        expectTrue(expectation.fulfilled)
      }
      
      tests.test("unfold awaiting next and cancel and throw") {
        let expectation = Expectation()
        let thrownError = SomeError()
        let task = Task.detached {
          let series = AsyncThrowingStream<String, Error>(
            unfolding: {
              withUnsafeCurrentTask { $0?.cancel() }
              throw thrownError
            }, onCancel: { expectation.fulfilled = true }
          )
          var iterator = series.makeAsyncIterator()
          do {
            _ = try await iterator.next(isolation: #isolation)
          } catch {
            if let failure = error as? SomeError {
              expectEqual(failure, thrownError)
            } else {
              expectUnreachable("unexpected error type")
            }
          }
        }
        _ = await task.getResult()
        expectTrue(expectation.fulfilled)
      }
      
      tests.test("unfold awaiting next with ignored cancellation throwing") {
        let task = Task.detached {
          let series = AsyncThrowingStream(unfolding: {
            withUnsafeCurrentTask { $0?.cancel() }
            return "hello"
          })
          var iterator = series.makeAsyncIterator()
          do {
            expectEqual(try await iterator.next(isolation: #isolation), "hello")
          } catch {
            expectUnreachable("unexpected error thrown")
          }
        }
        _ = await task.getResult()
      }

      tests.test("unfold awaiting next and throw with ignored cancellation") {
        let thrownError = SomeError()
        let task = Task.detached {
          let series = AsyncThrowingStream<String, Error>(unfolding: {
            withUnsafeCurrentTask { $0?.cancel() }
            throw thrownError
          })
          var iterator = series.makeAsyncIterator()
          do {
            _ = try await iterator.next(isolation: #isolation)
          } catch {
            if let failure = error as? SomeError {
              expectEqual(failure, thrownError)
            } else {
              expectUnreachable("unexpected error type")
            }
          }
        }
        _ = await task.getResult()
      }

      tests.test("unfold with early ignored cancellation throwing") {
        let task = Task.detached {
          let series = AsyncThrowingStream(unfolding: {
            return "hello"
          })
          var iterator = series.makeAsyncIterator()
          withUnsafeCurrentTask { $0?.cancel() }
          do {
            _ = try await iterator.next(isolation: #isolation)
          } catch {
            expectUnreachable("unexpected error thrown")
          }
        }
        _ = await task.getResult()
      }

      tests.test("unfold awaiting next and cancel before throw") {
        let expectation = Expectation()
        let task = Task.detached {
          let series = AsyncThrowingStream<String, Error>(
            unfolding: { throw SomeError() },
            onCancel: { expectation.fulfilled = true }
          )
          var iterator = series.makeAsyncIterator()
          do {
            withUnsafeCurrentTask { $0?.cancel() }
            expectEqual(try await iterator.next(isolation: #isolation), nil)
          } catch {
            expectUnreachable("unexpected error thrown")
          }
        }
        _ = await task.getResult()
        expectTrue(expectation.fulfilled)
      }

      tests.test("unfold and cancel before awaiting next throwing") {
        let expectation = Expectation()
        let task = Task.detached {
          let series = AsyncThrowingStream(
            unfolding: { return "hello" },
            onCancel: { expectation.fulfilled = true }
          )
          var iterator = series.makeAsyncIterator()
          do {
            withUnsafeCurrentTask { $0?.cancel() }
            expectEqual(try await iterator.next(isolation: #isolation), nil)
          } catch {
            expectUnreachable("unexpected error thrown")
          }
        }
        _ = await task.getResult()
        expectTrue(expectation.fulfilled)
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

      await runAllTestsAsync()
    }
  }
}


