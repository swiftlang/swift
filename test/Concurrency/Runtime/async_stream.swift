// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library)

// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib

// rdar://78109470
// UNSUPPORTED: back_deployment_runtime

// https://bugs.swift.org/browse/SR-14466
// UNSUPPORTED: OS=windows-msvc

// Race condition
// REQUIRES: rdar78033828

import _Concurrency
import StdlibUnittest
import Dispatch

struct SomeError: Error, Equatable {
  var value = Int.random(in: 0..<100)
}

var tests = TestSuite("AsyncStream")

@main struct Main {
  static func main() async {
    if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
      final class Expectation: UnsafeSendable {
        var fulfilled = false
      }

      tests.test("yield with no awaiting next") {
        let series = AsyncStream(String.self) { continuation in
          continuation.yield("hello")
        }
      }

      tests.test("yield with no awaiting next throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
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
          try await iterator.next()
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
        let series = AsyncStream(String.self) { continuation in
          detach {
            continuation.yield("hello")
          }
        }
      }

      tests.test("yield with no awaiting next detached throwing") {
        let series = AsyncThrowingStream(String.self) { continuation in
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
          try await iterator.next()
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
          let series = AsyncStream(String.self) { continuation in
            continuation.onTermination = { @Sendable _ in expectation.fulfilled = true }
          }
        }
        
        scopedLifetime(expectation)

        expectTrue(expectation.fulfilled)
      }

      tests.test("termination behavior on deinit with no values being awaited") {
        let expectation = Expectation()

        func scopedLifetime(_ expectation: Expectation) {
          let series = AsyncStream(String.self) { continuation in
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
          let series = AsyncStream(String.self) { continuation in
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
          let series = AsyncThrowingStream(String.self) { continuation in
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

      await runAllTestsAsync()
    }
  }
}


