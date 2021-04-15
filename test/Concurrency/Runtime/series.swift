// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib

import _Concurrency
import StdlibUnittest


struct SomeError: Error, Equatable {
  var value = Int.random(in: 0..<100)
}

let sleepInterval: UInt64 = 125_000_000
var tests = TestSuite("Series")

if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
  func forceBeingAsync() async -> Void { }

  tests.test("yield with no awaiting next") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        continuation.resume(yielding: "hello")
      }
      await forceBeingAsync()
    }
  }

  tests.test("yield with awaiting next") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        continuation.resume(yielding: "hello")
      }
      let iterator = series.makeAsyncIterator()
      expectEqual(await iterator.next(), "hello")
    }
  }

  tests.test("yield with awaiting next 2") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
      }
      let iterator = series.makeAsyncIterator()
      expectEqual(await iterator.next(), "hello")
      expectEqual(await iterator.next(), "world")
    }
  }

  tests.test("yield with awaiting next 2 and finish") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        continuation.resume(yielding: "hello")
        continuation.resume(yielding: "world")
        continuation.finish()
      }
      let iterator = series.makeAsyncIterator()
      expectEqual(await iterator.next(), "hello")
      expectEqual(await iterator.next(), "world")
      expectEqual(await iterator.next(), nil)
    }
  }

  tests.test("yield with no awaiting next detached") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        detach {
          continuation.resume(yielding: "hello")
        }
      }
      await forceBeingAsync()
    }
  }

  tests.test("yield with awaiting next detached") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        detach {
          continuation.resume(yielding: "hello")
        }
      }
      let iterator = series.makeAsyncIterator()
      expectEqual(await iterator.next(), "hello")
    }
  }

  tests.test("yield with awaiting next 2 detached") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        detach {
          continuation.resume(yielding: "hello")
          continuation.resume(yielding: "world")
        }
      }
      let iterator = series.makeAsyncIterator()
      expectEqual(await iterator.next(), "hello")
      expectEqual(await iterator.next(), "world")
    }
  }

  tests.test("yield with awaiting next 2 and finish detached") {
    runAsyncAndBlock {
      let series = Series(buffering: String.self) { continuation in
        detach {
          continuation.resume(yielding: "hello")
          continuation.resume(yielding: "world")
          continuation.finish()
        }
      }
      let iterator = series.makeAsyncIterator()
      expectEqual(await iterator.next(), "hello")
      expectEqual(await iterator.next(), "world")
      expectEqual(await iterator.next(), nil)
    }
  }
}

runAllTests()
