//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift Atomics open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Note: All atomic accesses on Wasm are sequentially consistent regardless of
// what ordering we tell LLVM to use.

import _SynchronizationShims

@_extern(c, "llvm.wasm.memory.atomic.wait32")
internal func _swift_stdlib_wait(
  on: UnsafePointer<UInt32>,
  expected: UInt32,
  timeout: Int64
) -> UInt32

@_extern(c, "llvm.wasm.memory.atomic.notify")
internal func _swift_stdlib_wake(on: UnsafePointer<UInt32>, count: UInt32) -> UInt32

extension Atomic where Value == _MutexHandle.State {
  internal borrowing func _wait(expected: _MutexHandle.State) {
    #if _runtime(_multithreaded)
    #if os(WASI)
    if _swift_stdlib_wasilibc_use_busy_futex_get() != 0 {
      // Note: On WebAssembly in a web browser, waiting on the main thread with
      // `memory.atomic.wait32` is not allowed. When wasi-libc busy-futex mode
      // is enabled, use bounded polling instead so `Mutex` remains usable on
      // the main thread for `wasm32-unknown-wasip1-threads`.
      var remaining: UInt32 = 1024
      while remaining > 0 {
        if load(ordering: .relaxed) != expected {
          return
        }
        remaining &-= 1
      }
      return
    }
    #endif
    _ = unsafe _swift_stdlib_wait(
      on: .init(_rawAddress),
      expected: expected.rawValue,

      // A timeout of < 0 means indefinitely.
      timeout: -1
    )
    #endif
  }

  internal borrowing func _wake() {
    #if _runtime(_multithreaded)
    // Only wake up 1 thread
    _ = unsafe _swift_stdlib_wake(on: .init(_rawAddress), count: 1)
    #endif
  }
}

@available(SwiftStdlib 6.0, *)
extension _MutexHandle {
  @available(SwiftStdlib 6.0, *)
  @frozen
  @usableFromInline
  internal enum State: UInt32, AtomicRepresentable {
    case unlocked
    case locked
    case contended
  }
}

@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline
  let storage: Atomic<State>

  @available(SwiftStdlib 6.0, *)
  @export(implementation)
  @_transparent
  public init() {
    storage = Atomic(.unlocked)
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _lock() {
    let (exchanged, state) = storage.compareExchange(
      expected: .unlocked,
      desired: .locked,
      successOrdering: .acquiring,
      failureOrdering: .relaxed
    )

    if _fastPath(exchanged) {
      // Locked!
      return
    }

    // If the mutex is already contended, go straight to waiting.
    if state == .contended {
      storage._wait(expected: .contended)
    }

    // Transition the mutex state into being contended. If the value stored
    // there was unlocked, then we acquired the lock. This has to store
    // .contended rather than .locked, even after being woken up, because other
    // threads may still be blocked in `_wait` and .contended is the only record
    // that makes the next `_unlock()` wake one of them. A weird quirk of this is
    // that we may go directly from .unlocked -> .contended when in fact the lock
    // is not contended, which only costs calling wake with no waiters.
    while storage.exchange(.contended, ordering: .acquiring) != .unlocked {
      // Block until unlock has been called. This will return early if the call
      // to unlock happened between attempting to acquire and attempting to
      // wait while nobody else managed to acquire it yet.
      storage._wait(expected: .contended)
    }

    // Locked!
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _tryLock() -> Bool {
    storage.compareExchange(
      expected: .unlocked,
      desired: .locked,
      successOrdering: .acquiring,
      failureOrdering: .relaxed
    ).exchanged
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _unlock() {
    // Transition our state from being either .locked or .contended to .unlocked.
    // At this point the mutex is freely acquirable. If the value that was
    // stored in the mutex was .locked, then no one else was waiting on this
    // mutex so we can just skip trying to wake up a thread.
    guard storage.exchange(.unlocked, ordering: .releasing) == .contended else {
      // Unlocked!
      return
    }

    // Otherwise, wake up our next lucky random thread to acquire the mutex.
    // (Assuming no new thread acquires the lock before it does)
    storage._wake()

    // Unlocked!
  }
}
