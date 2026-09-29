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

extension Atomic where Value == UInt32 {
  internal borrowing func _wait(expected: UInt32) {
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
      expected: expected,

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
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  @usableFromInline internal static var unlocked: UInt32 { 0 }
  @usableFromInline internal static var locked: UInt32 { 1 }
  @usableFromInline internal static var contended: UInt32 { 2 }

  @usableFromInline
  let storage: Atomic<UInt32>

  @available(SwiftStdlib 6.0, *)
  @export(implementation)
  @_transparent
  public init() {
    // A literal rather than `Self.unlocked`: reading the accessor cross-module
    // leaves a call in the initializer and `Mutex` stops being statically
    // initialized (test/SILOptimizer/static_atomics.swift).
    storage = Atomic(0)
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _lock() {
    // Note: We could probably merge this cas into a do/while style loop, but we
    // really want to perform the strong variant before attempting to do weak
    // ones in the loop.

    var (exchanged, state) = storage.compareExchange(
      expected: Self.unlocked,
      desired: Self.locked,
      successOrdering: .acquiring,
      failureOrdering: .relaxed
    )

    if _fastPath(exchanged) {
      // Locked!
      return
    }

    while !exchanged {
      // If we're not already contended, go ahead and transition the mutex state
      // into being contended. If when we do this that the value stored there
      // was unlocked, then we know we unintentionally acquired the lock. A
      // weird quirk that occurs if this happens is that we go directly from
      // unlocked -> contended when in fact the lock may not be contended.
      // We may be able to do another atomic access and change it to locked if
      // acquired it, but it may cause more problems than just potentially
      // calling wake with no waiters.
      if state != Self.contended, storage.exchange(
        Self.contended,
        ordering: .acquiring
      ) == Self.unlocked {
        // Locked!
        return
      }

      // Block until unlock has been called. This will return early if the call
      // to unlock happened between attempting to acquire and attempting to
      // wait while nobody else managed to acquire it yet.
      storage._wait(expected: Self.contended)

      (exchanged, state) = storage.weakCompareExchange(
        expected: Self.unlocked,
        desired: Self.locked,
        successOrdering: .acquiring,
        failureOrdering: .relaxed
      )
    }

    // Locked!
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _tryLock() -> Bool {
    storage.compareExchange(
      expected: Self.unlocked,
      desired: Self.locked,
      successOrdering: .acquiring,
      failureOrdering: .relaxed
    ).exchanged
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _unlock() {
    // Transition our state from being either locked or contended to unlocked.
    // At this point the mutex is freely acquirable. If the value that was
    // stored in the mutex was locked, then no one else was waiting on this
    // mutex so we can just skip trying to wake up a thread.
    let previous = storage.exchange(Self.unlocked, ordering: .releasing)
    guard previous == Self.contended else {
      // Unlocked!
      return
    }

    // Otherwise, wake up our next lucky random thread to acquire the mutex.
    // (Assuming no new thread acquires the lock before it does)
    storage._wake()

    // Unlocked!
  }
}
