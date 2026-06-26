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
//
// Plain-futex Mutex for FreeBSD: 3-state lock word, bounded spin, kernel
// fallback using UMTX_OP_WAIT_UINT_PRIVATE / UMTX_OP_WAKE_PRIVATE.
// Mirrors LinuxImpl.swift; see docs/SynchronizationMutexLinux.md for design.
//
// The previous implementation called UMTX_OP_MUTEX_LOCK on a raw uint32_t,
// which is incorrect: that op is for struct umutex. Under contention the kernel
// parked threads on the umutex wait-queue but the corresponding unlock never
// generated the right wakeup, causing a permanent deadlock. Fixed by using the
// plain-integer wait/wake pair (UMTX_OP_WAIT_UINT_PRIVATE / _WAKE_PRIVATE),
// which operates correctly on a bare uint32_t address.
//
//===----------------------------------------------------------------------===//

import _SynchronizationShims
import Glibc

@_extern(c, "llvm.readcyclecounter")
internal func _readCycleCounter() -> UInt64

@inline(__always)
internal func _cycleCounter() -> UInt64 {
#if arch(i386) || arch(x86_64)
  return _readCycleCounter()
#else
  return 0
#endif
}

extension Atomic where Value == UInt32 {
  // Sleeps while the underlying word equals `expected`. Returns 0 on a normal
  // wake, or the errno value. On FreeBSD, EBUSY (16) means the value changed
  // before we slept (analogous to Linux EAGAIN); EINTR (4) means a signal.
  internal borrowing func _futexWait(expected: UInt32) -> UInt32 {
    unsafe _swift_stdlib_futex_wait(.init(_rawAddress), expected)
  }

  // Wakes up to `count` waiters parked on this word.
  internal borrowing func _futexWake(count: UInt32) -> UInt32 {
    unsafe _swift_stdlib_futex_wake(.init(_rawAddress), count)
  }
}


@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  // Lock word states.
  @usableFromInline internal static var unlocked:  UInt32 { 0 }
  @usableFromInline internal static var locked:    UInt32 { 1 }
  @usableFromInline internal static var contended: UInt32 { 2 }

  @usableFromInline
  let storage: Atomic<UInt32>

  @usableFromInline
  let slowPathDepth: Atomic<UInt32>

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  public init() {
    storage = Atomic(0)
    slowPathDepth = Atomic(0)
  }
}

private var spinTries: UInt32 { 20 }
private var pauseBase: UInt32 { 64 }
private var maxActiveSpinners: UInt32 { 4 }

@available(SwiftStdlib 6.0, *)
extension _MutexHandle {
  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _lock() {
    let (exchanged, _) = storage.compareExchange(
      expected: Self.unlocked,
      desired: Self.locked,
      successOrdering: .acquiring,
      failureOrdering: .relaxed
    )

    if _fastPath(exchanged) {
      return
    }

    _lockSlow()
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _lockSlow() {
    let initialState = storage.load(ordering: .relaxed)
    let depth = slowPathDepth.load(ordering: .relaxed)
    let skipSpin = (initialState == Self.contended) && (depth >= maxActiveSpinners)

    if !skipSpin {
      let jitter = UInt32(truncatingIfNeeded: _cycleCounter())
      let mask = pauseBase &- 1
      var spinsRemaining = spinTries

      repeat {
        var state = storage.load(ordering: .relaxed)

        if state == Self.unlocked {
          let (exchanged, original) = storage.compareExchange(
            expected: Self.unlocked,
            desired: Self.locked,
            successOrdering: .acquiring,
            failureOrdering: .relaxed
          )
          if exchanged {
            return
          }
          state = original
        }

        if state == Self.contended { break }

        let pauses = pauseBase &+ (jitter & mask)
        for _ in 0 ..< pauses {
          _spinLoopHint()
        }

        spinsRemaining -= 1
      } while spinsRemaining > 0
    }

    var visibleToSpinners = false

    while true {
      if storage.exchange(Self.contended, ordering: .acquiring) == Self.unlocked {
        if visibleToSpinners {
          _ = slowPathDepth.wrappingSubtract(1, ordering: .relaxed)
        }
        return
      }

      if !visibleToSpinners {
        _ = slowPathDepth.wrappingAdd(1, ordering: .relaxed)
        visibleToSpinners = true
      }

      let waitResult = storage._futexWait(expected: Self.contended)
      switch waitResult {
      // 0:  woken by UMTX_OP_WAKE_PRIVATE
      // 16: EBUSY — value changed before we slept (lock released in the window)
      // 4:  EINTR — interrupted by a signal
      case 0, 16, 4:
        continue

      default:
        fatalError("Unknown error occurred while attempting to acquire a Mutex: \(waitResult)")
      }
    }
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    return storage.compareExchange(
      expected: Self.unlocked,
      desired: Self.locked,
      successOrdering: .acquiring,
      failureOrdering: .relaxed
    ).exchanged
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _unlock() {
    if storage.exchange(Self.unlocked, ordering: .releasing) == Self.locked {
      return
    }
    _unlockSlow()
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _unlockSlow() {
    _ = storage._futexWake(count: 1)
  }
}
