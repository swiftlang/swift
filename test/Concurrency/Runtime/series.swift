// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib

// https://bugs.swift.org/browse/SR-14466
// UNSUPPORTED: OS=windows-msvc

import _Concurrency
import StdlibUnittest
import Dispatch

@available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *)
extension TestSuite {
  @inline(never)
  public func test(
    _ name: String,
    file: String = #file, line: UInt = #line,
    _ testFunction: @escaping () async -> Void
  ) {
    test(name, file: file, line: line) {
      runAsyncAndBlock(testFunction)
    }
  }
}

struct SomeError: Error, Equatable {
  var value = Int.random(in: 0..<100)
}

var tests = TestSuite("Series")

if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
  final class Expectation: UnsafeSendable {
    var fulfilled = false
  }

  tests.test("yield with no awaiting next") {
    let series = Series(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
    }
  }

  tests.test("yield with no awaiting next throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
    }
  }

  tests.test("yield with awaiting next") {
    let series = Series(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
  }

  tests.test("yield with awaiting next throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
    }
    var iterator = series.makeAsyncIterator()
    do {
      expectEqual(try await iterator.next(), "hello")
    } catch {
      expectUnreachable("unexpected error thrown")
    }
  }

  tests.test("yield with awaiting next 2") {
    let series = Series(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
      continuation.resume(yielding: "world")
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
    expectEqual(await iterator.next(), "world")
  }

  tests.test("yield with awaiting next 2 throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
      continuation.resume(yielding: "world")
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
    let series = Series(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
      continuation.resume(yielding: "world")
      continuation.finish()
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
    expectEqual(await iterator.next(), "world")
    expectEqual(await iterator.next(), nil)
  }

  tests.test("yield with awaiting next 2 and finish throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
      continuation.resume(yielding: "world")
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
    let series = ThrowingSeries(buffering: String.self) { continuation in
      continuation.resume(yielding: "hello")
      continuation.resume(yielding: "world")
      continuation.finish(throwing: thrownError)
    }
    var iterator = series.makeAsyncIterator()
    do {
      expectEqual(try await iterator.next(), "hello")
      expectEqual(try await iterator.next(), "world")
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
    let series = Series(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
      }
    }
  }

  tests.test("yield with no awaiting next detached throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
      }
    }
  }

  tests.test("yield with awaiting next detached") {
    let series = Series(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
      }
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
  }

  tests.test("yield with awaiting next detached throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
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
    let series = Series(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
      }
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
    expectEqual(await iterator.next(), "world")
  }

  tests.test("yield with awaiting next 2 detached throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
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
    let series = Series(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
        continuation.finish()
      }
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
    expectEqual(await iterator.next(), "world")
    expectEqual(await iterator.next(), nil)
  }

  tests.test("yield with awaiting next 2 and finish detached throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
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
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
        continuation.finish(throwing: thrownError)
      }
    }
    var iterator = series.makeAsyncIterator()
    do {
      expectEqual(try await iterator.next(), "hello")
      expectEqual(try await iterator.next(), "world")
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
    let series = Series(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
        continuation.finish()
        continuation.resume(yielding: "This should not be emitted")
      }
    }
    var iterator = series.makeAsyncIterator()
    expectEqual(await iterator.next(), "hello")
    expectEqual(await iterator.next(), "world")
    expectEqual(await iterator.next(), nil)
    expectEqual(await iterator.next(), nil)
  }

  tests.test("yield with awaiting next 2 and finish detached with value after finish throwing") {
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
        continuation.finish()
        continuation.resume(yielding: "This should not be emitted")
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
    let series = ThrowingSeries(buffering: String.self) { continuation in
      detach {
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
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

  tests.test("cancellation behavior on deinit with no values being awaited") {
    let expectation = Expectation()

    func scopedLifetime(_ expectation: Expectation) {
      let series = Series(buffering: String.self) { continuation in
        continuation.onCancel = { expectation.fulfilled = true }
      }
    }
    
    scopedLifetime(expectation)

    expectTrue(expectation.fulfilled)
  }

  tests.test("cancellation behavior on deinit with no values being awaited throwing") {
    let expectation = Expectation()

    func scopedLifetime(_ expectation: Expectation) {
      let series = ThrowingSeries(buffering: String.self) { continuation in
        continuation.onCancel = { expectation.fulfilled = true }
      }
    }
    
    scopedLifetime(expectation)

    expectTrue(expectation.fulfilled)
  }

  tests.test("cancellation behavior of value emitted in handler") {
    let ready = DispatchSemaphore(value: 0)
    let done = DispatchSemaphore(value: 0)
    let task = detach {
      let series = Series(buffering: String.self) { continuation in
        continuation.onCancel = { continuation.resume(yielding: "Hit cancel") }
      }
      ready.signal()
      var iterator = series.makeAsyncIterator()
      let first = await iterator.next()
      expectEqual(first, "Hit cancel")
      let second = await iterator.next()
      expectEqual(second, nil)
      done.signal()
    }
    ready.wait()
    task.cancel()
    let result = done.wait(timeout: .now().advanced(by: .seconds(1)))
    switch result {
    case .timedOut:
      expectFalse(true, "Timeout when awaiting finished state")
    default: break
    }
  }

  tests.test("cancellation behavior of value emitted in handler throwing") {
    let ready = DispatchSemaphore(value: 0)
    let done = DispatchSemaphore(value: 0)
    let task = detach {
      let series = ThrowingSeries(buffering: String.self) { continuation in
        continuation.onCancel = { continuation.resume(yielding: "Hit cancel") }
      }
      ready.signal()
      var iterator = series.makeAsyncIterator()
      let first = await iterator.next()
      expectEqual(first, "Hit cancel")
      do {
        let second = try await iterator.next()
        expectEqual(second, nil)
      } catch {
        expectUnreachable("unexpected error thrown")
      }
      done.signal()
    }
    ready.wait()
    task.cancel()
    let result = done.wait(timeout: .now().advanced(by: .seconds(1)))
    switch result {
    case .timedOut:
      expectFalse(true, "Timeout when awaiting finished state")
    default: break
    }
  }
}

runAllTests()
