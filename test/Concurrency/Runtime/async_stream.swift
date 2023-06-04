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

var tests = TestSuite("AsyncStream")

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
        expectEqual(await iterator.next(), "hello")
      }

      tests.test("throwing factory method") {
        let (stream, continuation) = AsyncThrowingStream.makeStream(of: String.self, throwing: Error.self)
        continuation.yield("hello")

        var iterator = stream.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(), "hello")
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
        expectEqual(await iterator.next(), "hello")
      }

      tests.test("yield with awaiting next throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(), "hello")
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
        expectEqual(await iterator.next(), "hello")
        expectEqual(await iterator.next(), "world")
      }

      tests.test("yield with awaiting next 2 throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
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
        expectEqual(await iterator.next(), "hello")
        expectEqual(await iterator.next(), "world")
        expectEqual(await iterator.next(), nil)
      }

      tests.test("yield with awaiting next 2 and finish throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          continuation.yield("hello")
          continuation.yield("world")
          continuation.finish()
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          expectEqual(try await iterator.next(), nil)
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          _ = try await iterator.next()
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
        expectEqual(await iterator.next(), "hello")
      }

      tests.test("yield with awaiting next detached throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
          }
        }
        var iterator = series.makeAsyncIterator()
        do {
          expectEqual(try await iterator.next(), "hello")
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
        expectEqual(await iterator.next(), "hello")
        expectEqual(await iterator.next(), "world")
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
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
        expectEqual(await iterator.next(), "hello")
        expectEqual(await iterator.next(), "world")
        expectEqual(await iterator.next(), nil)
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          expectEqual(try await iterator.next(), nil)
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          _ = try await iterator.next()
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
        expectEqual(await iterator.next(), "hello")
        expectEqual(await iterator.next(), "world")
        expectEqual(await iterator.next(), nil)
        expectEqual(await iterator.next(), nil)
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          expectEqual(try await iterator.next(), nil)
          expectEqual(try await iterator.next(), nil)
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          expectEqual(try await iterator.next(), nil)
          expectEqual(try await iterator.next(), nil)
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
          expectEqual(try await iterator.next(), "hello")
          expectEqual(try await iterator.next(), "world")
          expectEqual(try await iterator.next(), nil)
          expectEqual(try await iterator.next(), nil)
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

      // Backpressure tests
      tests.test("SequenceDeinitialized when no iterator") {
        var (stream, source): (AsyncThrowingStream?, AsyncThrowingStream.Source) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 5, high: 10)
        )

        let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
        source.onTermination = {
            onTerminationContinuation.finish()
        }

        await withThrowingTaskGroup(of: Void.self) { group in
            group.addTask {
                while !Task.isCancelled {
                    onTerminationContinuation.yield()
                    try await Task.sleep(for: .seconds(0.2))
                }
            }

            var onTerminationIterator = onTerminationStream.makeAsyncIterator()
            _ = await onTerminationIterator.next()

            withExtendedLifetime(stream) {}
            stream = nil

            let terminationResult: Void? = await onTerminationIterator.next()
            expectNil(terminationResult)

            do {
                _ = try { try source.write(2) }()
                expectUnreachable("Expected an error to be thrown")
            } catch {
                expectTrue(error is AsyncStreamAlreadyFinishedError)
            }

            group.cancelAll()
        }
      }

      tests.test("SequenceDeinitialized when iterator") {
        do {
          var (stream, source): (AsyncThrowingStream?, AsyncThrowingStream.Source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          var iterator = stream?.makeAsyncIterator()

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source.onTermination = {
              onTerminationContinuation.finish()
          }

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              try withExtendedLifetime(stream) {
                  let writeResult = try source.write(1)
                  writeResult.assertIsProducerMore()
              }

              stream = nil

              do {
                  let writeResult = try { try source.write(2) }()
                  writeResult.assertIsProducerMore()
              } catch {
                print(error)
                  expectUnreachable("Expected no error to be thrown")
              }

              let element1 = try await iterator?.next()
              expectEqual(element1, 1)
              let element2 = try await iterator?.next()
              expectEqual(element2, 2)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("SequenceDeinitialized when finished") {
        var (stream, source): (AsyncThrowingStream?, AsyncThrowingStream.Source) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 5, high: 10)
        )

        let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
        source.onTermination = {
            onTerminationContinuation.finish()
        }

        await withThrowingTaskGroup(of: Void.self) { group in
            group.addTask {
                while !Task.isCancelled {
                    onTerminationContinuation.yield()
                    try await Task.sleep(for: .seconds(0.2))
                }
            }

            var onTerminationIterator = onTerminationStream.makeAsyncIterator()
            _ = await onTerminationIterator.next()

            withExtendedLifetime(stream) {
                source.finish(throwing: nil)
            }

            stream = nil

            let terminationResult: Void? = await onTerminationIterator.next()
            expectNil(terminationResult)

            do {
                _ = try { try source.write(1) }()
                expectUnreachable("Expected an error to be thrown")
            } catch {
                expectTrue(error is AsyncStreamAlreadyFinishedError)
            }

            group.cancelAll()
        }
      }

      tests.test("SequenceDeinitialized when streaming and suspended producer") {
        do {
          var (stream, source): (AsyncThrowingStream?, AsyncThrowingStream.Source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 2)
          )

          _ = try { try source.write(1) }()

          do {
              try await withCheckedThrowingContinuation { continuation in
                  source.write(1) { result in
                      continuation.resume(with: result)
                  }

                  stream = nil
                  _ = stream?.makeAsyncIterator()
              }
          } catch {
              expectTrue(error is AsyncStreamAlreadyFinishedError)
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorInitialized when initial") {
        let (stream, _) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 5, high: 10)
        )

        _ = stream.makeAsyncIterator()
      }

      tests.test("IteratorInitialized when streaming") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          try await source.write(1)

          var iterator = stream.makeAsyncIterator()
          let element = try await iterator.next()
          expectEqual(element, 1)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorInitialized when source finished") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          try await source.write(1)
          source.finish(throwing: nil)

          var iterator = stream.makeAsyncIterator()
          let element1 = try await iterator.next()
          expectEqual(element1, 1)
          let element2 = try await iterator.next()
          expectNil(element2)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorInitialized when finished") {
        do {
        let (stream, source) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 5, high: 10)
        )

        source.finish(throwing: nil)

        var iterator = stream.makeAsyncIterator()
        let element = try await iterator.next()
        expectNil(element)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorDeinitialized when initial") {
        do {
          var (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source.onTermination = {
              onTerminationContinuation.finish()
          }

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              iterator = nil
              _ = try await iterator?.next()

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorDeinitialized when streaming") {
        do {
          var (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source.onTermination = {
              onTerminationContinuation.finish()
          }

          try await source.write(1)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              iterator = nil
              _ = try await iterator?.next()

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorDeinitialized when source finished") {
        do {
          var (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source.onTermination = {
              onTerminationContinuation.finish()
          }

          try await source.write(1)
          source.finish(throwing: nil)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              iterator = nil
              _ = try await iterator?.next()

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorDeinitialized when finished") {
        do {
          var (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source.onTermination = {
              onTerminationContinuation.finish()
          }

          source.finish(throwing: nil)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              iterator = nil
              _ = try await iterator?.next()

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("IteratorDeinitialized when streaming and suspended producer") {
        do {
          var (stream, source): (AsyncThrowingStream?, AsyncThrowingStream.Source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 2)
          )

          var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream?.makeAsyncIterator()
          stream = nil

          _ = try { try source.write(1) }()

          do {
              try await withCheckedThrowingContinuation { continuation in
                  source.write(1) { result in
                      continuation.resume(with: result)
                  }

                  iterator = nil
              }
          } catch {
              expectTrue(error is AsyncStreamAlreadyFinishedError)
          }

          _ = try await iterator?.next()
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("SourceDeinitialized when initial") {
        var (stream, source): (AsyncThrowingStream, AsyncThrowingStream.Source?) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 5, high: 10)
        )

        let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
        source?.onTermination = {
            onTerminationContinuation.finish()
        }

        await withThrowingTaskGroup(of: Void.self) { group in
            group.addTask {
                while !Task.isCancelled {
                    onTerminationContinuation.yield()
                    try await Task.sleep(for: .seconds(0.2))
                }
            }

            var onTerminationIterator = onTerminationStream.makeAsyncIterator()
            _ = await onTerminationIterator.next()

            source = nil

            let terminationResult: Void? = await onTerminationIterator.next()
            expectNil(terminationResult)

            group.cancelAll()
        }

        withExtendedLifetime(stream) {}
      }

      tests.test("SourceDeinitialized when streaming and empty buffer") {
        do {
          var (stream, source): (AsyncThrowingStream, AsyncThrowingStream.Source?) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source?.onTermination = {
              onTerminationContinuation.finish()
          }

          try await source?.write(1)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              _ = try await iterator?.next()

              source = nil

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("SourceDeinitialized when streaming and not empty buffer") {
        do {
          var (stream, source): (AsyncThrowingStream, AsyncThrowingStream.Source?) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source?.onTermination = {
              onTerminationContinuation.finish()
          }

          try await source?.write(1)
          try await source?.write(2)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              _ = try await iterator?.next()

              source = nil

              _ = await onTerminationIterator.next()

              _ = try await iterator?.next()
              _ = try await iterator?.next()

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("SourceDeinitialized when source finished") {
        do {
          var (stream, source): (AsyncThrowingStream, AsyncThrowingStream.Source?) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 5, high: 10)
          )

          let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
          source?.onTermination = {
              onTerminationContinuation.finish()
          }

          try await source?.write(1)
          try await source?.write(2)
          source?.finish(throwing: nil)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while !Task.isCancelled {
                      onTerminationContinuation.yield()
                      try await Task.sleep(for: .seconds(0.2))
                  }
              }

              var onTerminationIterator = onTerminationStream.makeAsyncIterator()
              _ = await onTerminationIterator.next()

              var iterator: AsyncThrowingStream<Int, Error>.AsyncIterator? = stream.makeAsyncIterator()
              _ = try await iterator?.next()

              source = nil

              _ = await onTerminationIterator.next()

              _ = try await iterator?.next()
              _ = try await iterator?.next()

              let terminationResult: Void? = await onTerminationIterator.next()
              expectNil(terminationResult)

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("SourceDeinitialized when finished") {
        var (stream, source): (AsyncThrowingStream, AsyncThrowingStream.Source?) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 5, high: 10)
        )

        let (onTerminationStream, onTerminationContinuation) = AsyncStream<Void>.makeStream()
        source?.onTermination = {
            onTerminationContinuation.finish()
        }

        source?.finish(throwing: nil)

        await withThrowingTaskGroup(of: Void.self) { group in
            group.addTask {
                while !Task.isCancelled {
                    onTerminationContinuation.yield()
                    try await Task.sleep(for: .seconds(0.2))
                }
            }

            var onTerminationIterator = onTerminationStream.makeAsyncIterator()
            _ = await onTerminationIterator.next()

            _ = stream.makeAsyncIterator()

            source = nil

            _ = await onTerminationIterator.next()

            let terminationResult: Void? = await onTerminationIterator.next()
            expectNil(terminationResult)

            group.cancelAll()
        }
      }

      tests.test("SourceDeinitialized when streaming and suspended producer") {
        do {
          var (stream, source): (AsyncThrowingStream, AsyncThrowingStream.Source?) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 0, high: 0)
          )
          let (producerStream, producerContinuation) = AsyncThrowingStream<Void, Error>.makeStream()
          var iterator = stream.makeAsyncIterator()

          source?.write(1) {
              producerContinuation.yield(with: $0)
          }

          _ = try await iterator.next()
          source = nil

          do {
              try await producerStream.first { _ in true }
              expectUnreachable("We expected to throw here")
          } catch {
              expectTrue(error is AsyncStreamAlreadyFinishedError)
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Write when initial") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 5)
          )

          try await source.write(1)

          var iterator = stream.makeAsyncIterator()
          let element = try await iterator.next()
          expectEqual(element, 1)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Write when streaming and no consumer") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 5)
          )

          try await source.write(1)
          try await source.write(2)

          var iterator = stream.makeAsyncIterator()
          let element1 = try await iterator.next()
          expectEqual(element1, 1)
          let element2 = try await iterator.next()
          expectEqual(element2, 2)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Write when streaming and suspended consumer") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 5)
          )

          try await withThrowingTaskGroup(of: Int?.self) { group in
              group.addTask {
                  return try await stream.first { _ in true }
              }

              // This is always going to be a bit racy since we need the call to next() suspend
              try await Task.sleep(for: .seconds(0.5))

              try await source.write(1)
              let element = try await group.next()
              expectEqual(element, 1)
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Write when streaming and suspended consumer and empty sequence") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 5)
          )

          try await withThrowingTaskGroup(of: Int?.self) { group in
              group.addTask {
                  return try await stream.first { _ in true }
              }

              // This is always going to be a bit racy since we need the call to next() suspend
              try await Task.sleep(for: .seconds(0.5))

              try await source.write(contentsOf: [])
              try await source.write(contentsOf: [1])
              let element = try await group.next()
              expectEqual(element, 1)
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("EnqueueProducer when streaming and cancelled") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 2)
          )

          let (producerStream, producerSource) = AsyncThrowingStream<Void, Error>.makeStream()

          try await source.write(1)

          let writeResult =  try { try source.write(2) }()

          switch writeResult {
          case .produceMore:
              preconditionFailure()
          case .enqueueCallback(let callbackToken):
              source.cancelCallback(callbackToken: callbackToken)

              source.enqueueCallback(callbackToken: callbackToken) { result in
                  producerSource.yield(with: result)
              }
          }

          do {
              _ = try await producerStream.first { _ in true }
              expectUnreachable("Expected an error to be thrown")
          } catch {
              expectTrue(error is CancellationError)
          }

          let element = try await stream.first { _ in true }
          expectEqual(element, 1)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("EnqueueProducer when streaming and cancelled and async") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 2)
          )

          try await source.write(1)

          await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  try await source.write(2)
              }

              group.cancelAll()
              do {
                  try await group.next()
                  expectUnreachable("Expected an error to be thrown")
              } catch {
                  expectTrue(error is CancellationError)
              }
          }

          let element = try await stream.first { _ in true }
          expectEqual(element, 1)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("EnqueueProducer when streaming and interleaving") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 1)
          )
          var iterator = stream.makeAsyncIterator()

          let (producerStream, producerSource) = AsyncThrowingStream<Void, Error>.makeStream()

          let writeResult =  try { try source.write(1) }()

          switch writeResult {
          case .produceMore:
              preconditionFailure()
          case .enqueueCallback(let callbackToken):
              let element = try await iterator.next()
              expectEqual(element, 1)

              source.enqueueCallback(callbackToken: callbackToken) { result in
                  producerSource.yield(with: result)
              }
          }

          do {
              _ = try await producerStream.first { _ in true }
          } catch {
              expectUnreachable("Expected no error to be thrown")
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("EnqueueProducer when streaming and suspending") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 1)
          )
          var iterator = stream.makeAsyncIterator()

          let (producerStream, producerSource) = AsyncThrowingStream<Void, Error>.makeStream()

          let writeResult =  try { try source.write(1) }()

          switch writeResult {
          case .produceMore:
              preconditionFailure()
          case .enqueueCallback(let callbackToken):
              source.enqueueCallback(callbackToken: callbackToken) { result in
                  producerSource.yield(with: result)
              }
          }

          let element = try await iterator.next()
          expectEqual(element, 1)

          do {
              _ = try await producerStream.first { _ in true }
          } catch {
              expectUnreachable("Expected no error to be thrown")
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("EnqueueProducer when finished") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 1)
          )
          var iterator = stream.makeAsyncIterator()

          let (producerStream, producerSource) = AsyncThrowingStream<Void, Error>.makeStream()

          let writeResult =  try { try source.write(1) }()

          switch writeResult {
          case .produceMore:
              preconditionFailure()
          case .enqueueCallback(let callbackToken):
              source.finish(throwing: nil)

              source.enqueueCallback(callbackToken: callbackToken) { result in
                  producerSource.yield(with: result)
              }
          }

          let element = try await iterator.next()
          expectEqual(element, 1)

          do {
              _ = try await producerStream.first { _ in true }
              expectUnreachable("Expected an error to be thrown")
          } catch {
              expectTrue(error is AsyncStreamAlreadyFinishedError)
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("CancelProducer when streaming") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 2)
          )

          let (producerStream, producerSource) = AsyncThrowingStream<Void, Error>.makeStream()

          try await source.write(1)

          let writeResult =  try { try source.write(2) }()

          switch writeResult {
          case .produceMore:
              preconditionFailure()
          case .enqueueCallback(let callbackToken):
              source.enqueueCallback(callbackToken: callbackToken) { result in
                  producerSource.yield(with: result)
              }

              source.cancelCallback(callbackToken: callbackToken)
          }

          do {
              _ = try await producerStream.first { _ in true }
              expectUnreachable("Expected an error to be thrown")
          } catch {
              expectTrue(error is CancellationError)
          }

          let element = try await stream.first { _ in true }
          expectEqual(element, 1)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("CancelProducer when source finished") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 2)
          )

          let (producerStream, producerSource) = AsyncThrowingStream<Void, Error>.makeStream()

          try await source.write(1)

          let writeResult =  try { try source.write(2) }()

          switch writeResult {
          case .produceMore:
              preconditionFailure()
          case .enqueueCallback(let callbackToken):
              source.enqueueCallback(callbackToken: callbackToken) { result in
                  producerSource.yield(with: result)
              }

              source.finish(throwing: nil)

              source.cancelCallback(callbackToken: callbackToken)
          }

          do {
              _ = try await producerStream.first { _ in true }
              expectUnreachable("Expected an error to be thrown")
          } catch {
              expectTrue(error is AsyncStreamAlreadyFinishedError)
          }

          let element = try await stream.first { _ in true }
          expectEqual(element, 1)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Finish when streaming and consumer suspended") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 1, high: 1)
          )

          try await withThrowingTaskGroup(of: Int?.self) { group in
              group.addTask {
                  return try await stream.first { $0 == 2 }
              }

              // This is always going to be a bit racy since we need the call to next() suspend
              try await Task.sleep(for: .seconds(0.5))

              source.finish(throwing: nil)
              let element = try await group.next()
              expectEqual(element, .some(nil))
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Finish when initial") {
        let (stream, source) = AsyncThrowingStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 1, high: 1)
        )

        source.finish(throwing: CancellationError())

        do {
            for try await _ in stream {}
            expectUnreachable("Expected an error to be thrown")
        } catch {
            expectTrue(error is CancellationError)
        }
      }

      tests.test("Backpressure async") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 4)
          )

          let (backPressureEventStream, backPressureEventContinuation) = AsyncStream.makeStream(of: Void.self)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  while true {
                      backPressureEventContinuation.yield(())
                      try await source.write(contentsOf: [1])
                  }
              }

              var backPressureEventIterator = backPressureEventStream.makeAsyncIterator()
              var iterator = stream.makeAsyncIterator()

              await backPressureEventIterator.next()
              await backPressureEventIterator.next()
              await backPressureEventIterator.next()
              await backPressureEventIterator.next()

              _ = try await iterator.next()
              _ = try await iterator.next()
              _ = try await iterator.next()

              await backPressureEventIterator.next()
              await backPressureEventIterator.next()
              await backPressureEventIterator.next()

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Backpressure sync") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 4)
          )

          let (backPressureEventStream, backPressureEventContinuation) = AsyncStream.makeStream(of: Void.self)

          try await withThrowingTaskGroup(of: Void.self) { group in
              group.addTask {
                  @Sendable func yield() {
                      backPressureEventContinuation.yield(())
                      source.write(contentsOf: [1]) { result in
                          switch result {
                          case .success:
                              yield()

                          case .failure:
                              break
                          }
                      }
                  }

                  yield()
              }

              var backPressureEventIterator = backPressureEventStream.makeAsyncIterator()
              var iterator = stream.makeAsyncIterator()

              await backPressureEventIterator.next()
              await backPressureEventIterator.next()
              await backPressureEventIterator.next()
              await backPressureEventIterator.next()

              _ = try await iterator.next()
              _ = try await iterator.next()
              _ = try await iterator.next()

              await backPressureEventIterator.next()
              await backPressureEventIterator.next()
              await backPressureEventIterator.next()

              group.cancelAll()
          }
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("Throws error") {
        do {
          let (stream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 4)
          )

          try await source.write(1)
          try await source.write(2)
          source.finish(throwing: CancellationError())

          var elements = [Int]()
          var iterator = stream.makeAsyncIterator()

          do {
              while let element = try await iterator.next() {
                  elements.append(element)
              }
              expectUnreachable("Expected an error to be thrown")
          } catch {
              expectTrue(error is CancellationError)
              expectEqual(elements, [1, 2])
          }

          let element = try await iterator.next()
          expectNil(element)
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("AsyncSequence write") {
        do {
          let (stream, continuation) = AsyncStream<Int>.makeStream()
          let (backpressuredStream, source) = AsyncThrowingStream.makeStream(
              of: Int.self,
              backPressureStrategy: .watermark(low: 2, high: 4)
          )

          continuation.yield(1)
          continuation.yield(2)
          continuation.finish()

          try await source.write(contentsOf: stream)
          source.finish(throwing: nil)

          let elements = try await backpressuredStream.collect()
          expectEqual(elements, [1, 2])
        } catch {
            expectUnreachable("Expected no error to be thrown")
        }
      }

      tests.test("AsyncStream backpressure async") {
        let (stream, source) = AsyncStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 2, high: 4)
        )

        let (backPressureEventStream, backPressureEventContinuation) = AsyncStream.makeStream(of: Void.self)

        await withTaskGroup(of: Void.self) { group in
            group.addTask {
                @Sendable func yield() {
                    backPressureEventContinuation.yield(())
                    source.write(contentsOf: [1]) { result in
                        switch result {
                        case .success:
                            yield()

                        case .failure:
                            break
                        }
                    }
                }

                yield()
            }

            var backPressureEventIterator = backPressureEventStream.makeAsyncIterator()
            var iterator = stream.makeAsyncIterator()

            await backPressureEventIterator.next()
            await backPressureEventIterator.next()
            await backPressureEventIterator.next()
            await backPressureEventIterator.next()

            _ = await iterator.next()
            _ = await iterator.next()
            _ = await iterator.next()

            await backPressureEventIterator.next()
            await backPressureEventIterator.next()
            await backPressureEventIterator.next()

            group.cancelAll()
        }
      }

      tests.test("Backpressure sync") {
        let (stream, source) = AsyncStream.makeStream(
            of: Int.self,
            backPressureStrategy: .watermark(low: 2, high: 4)
        )

        let (backPressureEventStream, backPressureEventContinuation) = AsyncStream.makeStream(of: Void.self)

        await withTaskGroup(of: Void.self) { group in
            group.addTask {
                @Sendable func yield() {
                    backPressureEventContinuation.yield(())
                    source.write(contentsOf: [1]) { result in
                        switch result {
                        case .success:
                            yield()

                        case .failure:
                            break
                        }
                    }
                }

                yield()
            }

            var backPressureEventIterator = backPressureEventStream.makeAsyncIterator()
            var iterator = stream.makeAsyncIterator()

            await backPressureEventIterator.next()
            await backPressureEventIterator.next()
            await backPressureEventIterator.next()
            await backPressureEventIterator.next()

            _ = await iterator.next()
            _ = await iterator.next()
            _ = await iterator.next()

            await backPressureEventIterator.next()
            await backPressureEventIterator.next()
            await backPressureEventIterator.next()

            group.cancelAll()
        }
      }

      await runAllTestsAsync()
    }
  }
}

extension AsyncSequence {
    /// Collect all elements in the sequence into an array.
    fileprivate func collect() async rethrows -> [Element] {
        try await self.reduce(into: []) { accumulated, next in
            accumulated.append(next)
        }
    }
}

extension AsyncThrowingStream.Source.WriteResult {
    func assertIsProducerMore() {
        switch self {
        case .produceMore:
            return

        case .enqueueCallback:
            expectUnreachable("Expected produceMore")
        }
    }

    func assertIsEnqueueCallback() {
        switch self {
        case .produceMore:
            expectUnreachable("Expected enqueueCallback")

        case .enqueueCallback:
            return
        }
    }
}


