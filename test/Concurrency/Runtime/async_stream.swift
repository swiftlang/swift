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
        let (stream, continuation) = AsyncStream<String>.makeStream(
          bufferingPolicy: .bufferingOldest(3)
        )
        let r1 = continuation.yield("a")
        let r2 = continuation.yield("b")
        let r3 = continuation.yield("c") // buffer now full
        let r4 = continuation.yield("d") // should be dropped
        let r5 = continuation.yield("e") // should be dropped
        continuation.finish()

        if case .enqueued(let remaining) = r1 { expectEqual(remaining, 2) }
        else { expectUnreachable("expected .enqueued(2) for r1") }
        if case .enqueued(let remaining) = r2 { expectEqual(remaining, 1) }
        else { expectUnreachable("expected .enqueued(1) for r2") }
        if case .enqueued(let remaining) = r3 { expectEqual(remaining, 0) }
        else { expectUnreachable("expected .enqueued(0) for r3") }
        if case .dropped(let value) = r4 { expectEqual(value, "d") }
        else { expectUnreachable("expected .dropped(d) for r4") }
        if case .dropped(let value) = r5 { expectEqual(value, "e") }
        else { expectUnreachable("expected .dropped(e) for r5") }

        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "a")
        expectEqual(await iterator.next(isolation: #isolation), "b")
        expectEqual(await iterator.next(isolation: #isolation), "c")
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("bufferingNewest semantics") {
        let (stream, continuation) = AsyncStream.makeStream(
          of: String.self,
          bufferingPolicy: .bufferingNewest(3)
        )
        let r1 = continuation.yield("a")
        let r2 = continuation.yield("b")
        let r3 = continuation.yield("c")
        let r4 = continuation.yield("d") // full: evicts oldest "a"
        let r5 = continuation.yield("e") // full: evicts oldest "b"
        continuation.finish()

        if case .enqueued(let remaining) = r1 { expectEqual(remaining, 2) }
        else { expectUnreachable("expected .enqueued(2) for r1") }
        if case .enqueued(let remaining) = r2 { expectEqual(remaining, 1) }
        else { expectUnreachable("expected .enqueued(1) for r2") }
        if case .enqueued(let remaining) = r3 { expectEqual(remaining, 0) }
        else { expectUnreachable("expected .enqueued(0) for r3") }
        // The dropped value is the evicted oldest, not the incoming element
        if case .dropped(let value) = r4 { expectEqual(value, "a") }
        else { expectUnreachable("expected .dropped(a) for r4") }
        if case .dropped(let value) = r5 { expectEqual(value, "b") }
        else { expectUnreachable("expected .dropped(b) for r5") }

        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "c")
        expectEqual(await iterator.next(isolation: #isolation), "d")
        expectEqual(await iterator.next(isolation: #isolation), "e")
        expectEqual(await iterator.next(isolation: #isolation), nil)
      }

      tests.test("buffering policy semantics throwing") {
        // bufferingOldest(2): first two kept, third dropped (incoming is dropped)
        let (oldestStream, oldestCont) = AsyncThrowingStream<String, Error>.makeStream(
          bufferingPolicy: .bufferingOldest(2)
        )
        _ = oldestCont.yield("a")
        _ = oldestCont.yield("b")
        let oldestDropped = oldestCont.yield("c")
        if case .dropped(let value) = oldestDropped { expectEqual(value, "c") }
        else { expectUnreachable("expected .dropped(c) for oldest overflow") }
        oldestCont.finish()
        do {
          var oldestIt = oldestStream.makeAsyncIterator()
          expectEqual(try await oldestIt.next(isolation: #isolation), "a")
          expectEqual(try await oldestIt.next(isolation: #isolation), "b")
          expectEqual(try await oldestIt.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingOldest throwing test")
        }

        // bufferingNewest(2): oldest evicted when full, newest kept
        let (newestStream, newestCont) = AsyncThrowingStream<String, Error>.makeStream(
          bufferingPolicy: .bufferingNewest(2)
        )
        _ = newestCont.yield("x")
        _ = newestCont.yield("y")
        let newestDropped = newestCont.yield("z")
        if case .dropped(let value) = newestDropped { expectEqual(value, "x") }
        else { expectUnreachable("expected .dropped(x) eviction for newest overflow") }
        newestCont.finish()
        do {
          var newestIt = newestStream.makeAsyncIterator()
          expectEqual(try await newestIt.next(isolation: #isolation), "y")
          expectEqual(try await newestIt.next(isolation: #isolation), "z")
          expectEqual(try await newestIt.next(isolation: #isolation), nil)
        } catch {
          expectUnreachable("unexpected error in bufferingNewest throwing test")
        }
      }

      tests.test("buffering zero capacity drops all") {
        // bufferingOldest(0): every yield dropped immediately
        let (oldestStream, oldestCont) = AsyncStream<Int>.makeStream(
          bufferingPolicy: .bufferingOldest(0)
        )
        if case .dropped(let v) = oldestCont.yield(1) { expectEqual(v, 1) }
        else { expectUnreachable("expected .dropped(1) for bufferingOldest(0)") }
        if case .dropped(let v) = oldestCont.yield(2) { expectEqual(v, 2) }
        else { expectUnreachable("expected .dropped(2) for bufferingOldest(0)") }
        oldestCont.finish()
        var oldestIt = oldestStream.makeAsyncIterator()
        expectEqual(await oldestIt.next(isolation: #isolation), nil)

        // bufferingNewest(0): every yield dropped immediately
        let (newestStream, newestCont) = AsyncStream.makeStream(
          of: Int.self,
          bufferingPolicy: .bufferingNewest(0)
        )
        if case .dropped(let v) = newestCont.yield(3) { expectEqual(v, 3) }
        else { expectUnreachable("expected .dropped(3) for bufferingNewest(0)") }
        if case .dropped(let v) = newestCont.yield(4) { expectEqual(v, 4) }
        else { expectUnreachable("expected .dropped(4) for bufferingNewest(0)") }
        newestCont.finish()
        var newestIt = newestStream.makeAsyncIterator()
        expectEqual(await newestIt.next(isolation: #isolation), nil)
      }

      tests.test("buffering negative capacity drops all") {
        // bufferingOldest(-1): every yield dropped immediately
        let (oldestStream, oldestCont) = AsyncStream<Int>.makeStream(
          bufferingPolicy: .bufferingOldest(-1)
        )
        if case .dropped(let v) = oldestCont.yield(1) { expectEqual(v, 1) }
        else { expectUnreachable("expected .dropped(1) for bufferingOldest(0)") }
        if case .dropped(let v) = oldestCont.yield(2) { expectEqual(v, 2) }
        else { expectUnreachable("expected .dropped(2) for bufferingOldest(0)") }
        oldestCont.finish()
        var oldestIt = oldestStream.makeAsyncIterator()
        expectEqual(await oldestIt.next(isolation: #isolation), nil)

        // bufferingNewest(-1): every yield dropped immediately
        let (newestStream, newestCont) = AsyncStream.makeStream(
          of: Int.self,
          bufferingPolicy: .bufferingNewest(-1)
        )
        if case .dropped(let v) = newestCont.yield(3) { expectEqual(v, 3) }
        else { expectUnreachable("expected .dropped(3) for bufferingNewest(0)") }
        if case .dropped(let v) = newestCont.yield(4) { expectEqual(v, 4) }
        else { expectUnreachable("expected .dropped(4) for bufferingNewest(0)") }
        newestCont.finish()
        var newestIt = newestStream.makeAsyncIterator()
        expectEqual(await newestIt.next(isolation: #isolation), nil)
      }

      // MARK: - YieldResult Inspection

      tests.test("yield result enqueued remaining") {
        // Unbounded: remaining is always Int.max
        let (stream, cont) = AsyncStream<String>.makeStream()
        let r = cont.yield("hello")
        if case .enqueued(let remaining) = r { expectEqual(remaining, Int.max) }
        else { expectUnreachable("expected .enqueued(Int.max) for unbounded stream") }
        _ = stream

        // Bounded: remaining decrements as buffer fills
        let (bStream, bCont) = AsyncStream.makeStream(
          of: String.self,
          bufferingPolicy: .bufferingOldest(3)
        )
        let b1 = bCont.yield("a")
        let b2 = bCont.yield("b")
        let b3 = bCont.yield("c")
        let b4 = bCont.yield("d") // buffer full
        if case .enqueued(let r1) = b1 { expectEqual(r1, 2) }
        else { expectUnreachable("expected .enqueued(2) for b1") }
        if case .enqueued(let r2) = b2 { expectEqual(r2, 1) }
        else { expectUnreachable("expected .enqueued(1) for b2") }
        if case .enqueued(let r3) = b3 { expectEqual(r3, 0) }
        else { expectUnreachable("expected .enqueued(0) for b3") }
        if case .dropped = b4 { }
        else { expectUnreachable("expected .dropped for b4 when buffer full") }
        _ = bStream
      }

      tests.test("yield result terminated") {
        // AsyncStream: yields after finish return .terminated
        let (stream, cont) = AsyncStream<String>.makeStream()
        cont.finish()
        let r = cont.yield("after finish")
        if case .terminated = r { }
        else { expectUnreachable("expected .terminated for yield after finish") }
        _ = stream

        // AsyncThrowingStream: yields after finish(throwing:) return .terminated
        let (tStream, tCont) = AsyncThrowingStream<String, Error>.makeStream()
        tCont.finish(throwing: SomeError())
        let tr = tCont.yield("after throw")
        if case .terminated = tr { }
        else { expectUnreachable("expected .terminated for yield after finish(throwing:)") }
        _ = tStream
      }

      // MARK: - Alternative Yield Methods

      tests.test("yield with success result") {
        // AsyncStream
        let (stream, cont) = AsyncStream<String>.makeStream()
        _ = cont.yield(with: .success("hello"))
        cont.finish()
        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), "hello")
        expectEqual(await iterator.next(isolation: #isolation), nil)

        // AsyncThrowingStream
        let (tStream, tCont) = AsyncThrowingStream<String, Error>.makeStream()
        _ = tCont.yield(with: .success("world"))
        tCont.finish()
        var tIterator = tStream.makeAsyncIterator()
        do {
          expectEqual(try await tIterator.next(isolation: #isolation), "world")
          expectEqual(try await tIterator.next(isolation: #isolation), nil)
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
        // AsyncStream<Void>
        do {
          let (stream, cont) = AsyncStream<Void>.makeStream()
          let result = cont.yield()
          cont.finish()

          if case .enqueued = result { }
          else { expectUnreachable("expected .enqueued for void yield") }

          var iterator = stream.makeAsyncIterator()
          let value: Void? = await iterator.next(isolation: #isolation)
          expectNotNil(value)
          expectNil(await iterator.next(isolation: #isolation))
        }

        // AsyncThrowingStream<Void, Error>
        do {
          let (stream, cont) = AsyncThrowingStream<Void, Error>.makeStream()
          let result = cont.yield()
          cont.finish()

          if case .enqueued = result { }
          else { expectUnreachable("expected .enqueued for void yield") }

          var iterator = stream.makeAsyncIterator()
          do {
            let value: Void? = try await iterator.next(isolation: #isolation)
            expectTrue(value != nil)
            expectNil(try await iterator.next(isolation: #isolation))
          } catch {
            expectUnreachable("unexpected error thrown")
          }
        }
      }

      // MARK: - Unfolding Initializer

      tests.test("unfolding init basic") {
        // AsyncStream: counter closure yields 1, 2, 3, then terminates
        nonisolated(unsafe) var counter1 = 0
        let stream = AsyncStream<Int>(unfolding: {
          counter1 += 1
          return counter1 <= 3 ? counter1 : nil
        })
        var iterator = stream.makeAsyncIterator()
        expectEqual(await iterator.next(isolation: #isolation), 1)
        expectEqual(await iterator.next(isolation: #isolation), 2)
        expectEqual(await iterator.next(isolation: #isolation), 3)
        expectEqual(await iterator.next(isolation: #isolation), nil)

        // AsyncThrowingStream: same with try await
        nonisolated(unsafe) var counter2 = 0
        let tStream = AsyncThrowingStream<Int, Error>(unfolding: {
          counter2 += 1
          return counter2 <= 3 ? counter2 : nil
        })
        var tIterator = tStream.makeAsyncIterator()
        do {
          expectEqual(try await tIterator.next(isolation: #isolation), 1)
          expectEqual(try await tIterator.next(isolation: #isolation), 2)
          expectEqual(try await tIterator.next(isolation: #isolation), 3)
          expectEqual(try await tIterator.next(isolation: #isolation), nil)
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

      // MARK: - Task Cancellation

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

      // MARK: - onTermination

      tests.test("onTermination finished reason") {
        let expectation = Expectation()
        let (_, continuation) = AsyncStream<String>.makeStream()
        continuation.onTermination = { @Sendable reason in
          if case .finished = reason { expectation.fulfilled = true }
        }
        continuation.finish()
        expectTrue(expectation.fulfilled)
      }

      tests.test("onTermination throwing finished reasons") {
        // Case A: finish() with no error -> .finished(nil)
        let nilExpectation = Expectation()
        let (_, contA) = AsyncThrowingStream<String, Error>.makeStream()
        contA.onTermination = { @Sendable reason in
          if case .finished(let e) = reason, e == nil { nilExpectation.fulfilled = true }
        }
        contA.finish()
        expectTrue(nilExpectation.fulfilled)

        // Case B: finish(throwing:) -> .finished(error)
        let errExpectation = Expectation()
        let thrownError = SomeError()
        let (_, contB) = AsyncThrowingStream<String, Error>.makeStream()
        contB.onTermination = { @Sendable reason in
          if case .finished(let e) = reason,
             let failure = e as? SomeError,
             failure == thrownError {
            errExpectation.fulfilled = true
          }
        }
        contB.finish(throwing: thrownError)
        expectTrue(errExpectation.fulfilled)
      }

      tests.test("onTermination called exactly once") {
        // AsyncStream: finish() called twice; handler invoked only once
        nonisolated(unsafe) var counter = 0
        let (_, cont) = AsyncStream<String>.makeStream()
        cont.onTermination = { @Sendable _ in counter += 1 }
        cont.finish()
        cont.finish() // second call is a no-op; handler must not fire again
        // TODO: if you reset the handler before calling finish again it will be called
        // should we prevent that?
        expectEqual(counter, 1)

        // AsyncThrowingStream: same behavior
        nonisolated(unsafe) var tCounter = 0
        let (_, tCont) = AsyncThrowingStream<String, Error>.makeStream()
        tCont.onTermination = { @Sendable _ in tCounter += 1 }
        tCont.finish()
        tCont.finish()
        expectEqual(tCounter, 1)
      }

      // MARK: - for try await

      tests.test("for try await") {
        // Normal completion: all yielded values collected
        let (streamA, contA) = AsyncThrowingStream<String, Error>.makeStream()
        contA.yield("a")
        contA.yield("b")
        contA.finish()
        var collected: [String] = []
        do {
          for try await value in streamA { collected.append(value) }
        } catch {
          expectUnreachable("unexpected error in normal completion")
        }
        expectEqual(collected, ["a", "b"])

        // Error propagation: values before throw are collected; error is caught
        let (streamB, contB) = AsyncThrowingStream<String, Error>.makeStream()
        let thrownError = SomeError()
        contB.yield("x")
        contB.finish(throwing: thrownError)
        var collectedB: [String] = []
        do {
          for try await value in streamB { collectedB.append(value) }
          expectUnreachable("expected thrown error")
        } catch {
          if let failure = error as? SomeError {
            expectEqual(failure, thrownError)
          } else {
            expectUnreachable("unexpected error type")
          }
        }
        expectEqual(collectedB, ["x"])
      }

      // // MARK: - Parity

      tests.test("finish idempotence non-throwing") {
        let series = AsyncStream(String.self) { continuation in
          nonisolated(unsafe) var terminalCallCount = 0
          continuation.onTermination = { _ in terminalCallCount += 1 }
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
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          _ = AsyncThrowingStream(String.self) { continuation in
            continuation.onTermination = { @Sendable _ in expectation.fulfilled = true }
            continuation.finish()
          }
        }

        scopedLifetime(expectation)
        expectTrue(expectation.fulfilled)
      }

      // This test documents the current single-consumer limitation of AsyncThrowingStream.
      tests.test("finish behavior with multiple consumers throwing") {
        expectCrashLater()
        let (stream, continuation) = AsyncThrowingStream<Int, Error>.makeStream()
        let (controlStream, controlContinuation) = AsyncStream<Int>.makeStream()
        var controlIterator = controlStream.makeAsyncIterator()

        func makeConsumingTaskWithIndex(_ index: Int) -> Task<Void, Error> {
          Task { @MainActor in
            controlContinuation.yield(index)
            for try await _ in stream {}
          }
        }

        let consumer1 = makeConsumingTaskWithIndex(1)
        expectEqual(await controlIterator.next(isolation: #isolation), 1)

        // Starting consumer2 while consumer1 is suspended on next() triggers fatalError
        let consumer2 = makeConsumingTaskWithIndex(2)
        expectEqual(await controlIterator.next(isolation: #isolation), 2)

        await MainActor.run {}
        continuation.finish()

        do {
          _ = try await consumer1.value
          _ = try await consumer2.value
        } catch {
          expectUnreachable("unexpected error in multiple consumers test")
        }
      }

      await runAllTestsAsync()
    }
  }
}


