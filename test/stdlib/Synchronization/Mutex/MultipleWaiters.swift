// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: synchronization
// UNSUPPORTED: threading_none

import Synchronization
import StdlibUnittest
import SwiftPrivateThreadExtras

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(Android)
import Android
#elseif os(WASI)
import WASILibc
#elseif os(Windows)
import WinSDK
#endif

@available(SwiftStdlib 6.0, *)
final class Shared: Sendable {
  let mutex = Mutex(0)
  let arrivals = Atomic(0)
}

func sleep(milliseconds: Int) {
#if os(Windows)
  Sleep(DWORD(milliseconds))
#else
  var duration = timespec(tv_sec: 0, tv_nsec: milliseconds * 1_000_000)
  nanosleep(&duration, nil)
#endif
}

let suite = TestSuite("MutexMultipleWaiters")

if #available(SwiftStdlib 6.0, *) {
  // https://github.com/swiftlang/swift/issues/92583
  suite.test("UnlockWakesEveryWaiter") {
    let waiterCount = 2
    let shared = Shared()

    let waiters = shared.mutex.withLock { _ in
      let waiters = (0 ..< waiterCount).map { _ in
        let (result, thread) = _stdlib_thread_create_block({ _ in
          shared.arrivals.add(1, ordering: .relaxed)
          shared.mutex.withLock { $0 += 1 }
        }, ())
        expectEqual(0, result)
        // The handle is non-nil whenever thread creation succeeds.
        return thread!
      }

      // Keep holding the mutex until every waiter has blocked in `lock()`.
      while shared.arrivals.load(ordering: .relaxed) < waiterCount {}
      sleep(milliseconds: 100)
      return waiters
    }

    // A lost wakeup leaves a waiter blocked forever, so this join hangs.
    for waiter in waiters {
      _ = _stdlib_thread_join(waiter, Void.self)
    }
    expectEqual(waiterCount, shared.mutex.withLock { $0 })
  }
}

runAllTests()
