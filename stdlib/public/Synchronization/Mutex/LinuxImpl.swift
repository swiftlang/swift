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
// Plain-futex Mutex: 3-state lock word, bounded spin, kernel fallback.
// See: docs/SynchronizationMutexLinux.md
//
//===----------------------------------------------------------------------===//

import _SynchronizationShims
#if canImport(Android)
import Android
#elseif canImport(Musl)
import Musl
#else
import Glibc
#endif

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
  // wake or the errno value (`EAGAIN=11` and `EINTR=4` are the expected retryable
  // cases).
  internal borrowing func _futexWait(expected: UInt32) -> UInt32 {
    unsafe _swift_stdlib_futex_wait(.init(_rawAddress), expected)
  }

  // Wakes up to `count` waiters parked on this word. Result is the number
  // woken (or errno on failure); callers typically discard it.
  internal borrowing func _futexWake(count: UInt32) -> UInt32 {
    unsafe _swift_stdlib_futex_wake(.init(_rawAddress), count)
  }
}


@available(SwiftStdlib 6.0, *)
@frozen
@_staticExclusiveOnly
public struct _MutexHandle: ~Copyable {
  // Lock word states.
  @usableFromInline internal static var unlocked:  UInt32 { 0 } // no owner
  @usableFromInline internal static var locked:    UInt32 { 1 } // held, no waiters parked in kernel
  @usableFromInline internal static var contended: UInt32 { 2 } // held, at least one waiter parked in kernel

  @usableFromInline
  let storage: Atomic<UInt32>

  // Approximate count of threads currently in `_lockSlow`'s kernel phase. Read by the entry depth gate;
  // inexact (e.g. counts briefly run between wake and retry), but used only as a hint.
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

// Spin-phase iteration budget.
private var spinTries: UInt32 { 20 }

// CPU pauses per spin iteration, before jitter. Must be a power of two (masked to generate the jitter via
// `jitter & (pauseBase - 1)`).
private var pauseBase: UInt32 { 64 }

// Once this many threads are already waiting for the lock, new arrivals skip the spin loop and go to sleep
// immediately. Keeps the set of actively-spinning threads bounded so the lock holder's critical section runs
// without cache-line interference from the spinners.
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
      // Locked!
      return
    }

    _lockSlow()
  }

  // Slow path for `_lock`:
  //   - Depth gate: if the queue is already deep, skip straight to parking. Bounds tail latency and keeps the spinner
  //     pool small so the owner's critical section runs uncontested.
  //   - Spin: bounded pause-based spin with per-thread jitter. Stops early if the lock is observed contended, so we
  //     don't steal it from a thread the kernel is about to wake.
  //   - Kernel: loop try-acquire plus `FUTEX_WAIT` until acquired. A thread that has to park bumps `slowPathDepth` on
  //     its first failed try-acquire and drops it on successful acquire. This feeds the depth gate: once enough
  //     threads are parked, new arrivals skip spinning and park directly, which stops spinners from stealing the
  //     lock out from under parked threads and bounds tail latency. Threads that win the initial try-acquire never
  //     touch the counter.
  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _lockSlow() {
    // Before relinquishing control to the kernel to block this particular
    // thread, run a little spin lock to keep this thread busy in the scenario
    // where the current owner thread's critical section is somewhat quick. We
    // avoid a lot of the syscall overhead in these cases which allow both the
    // owner thread and this current thread to do the user-space atomic for
    // releasing and acquiring (assuming no existing waiters).

    // Skip the spin when the queue is already deep - extra spinners just slow down the parked threads' handoff.
    let initialState = storage.load(ordering: .relaxed)
    let depth = slowPathDepth.load(ordering: .relaxed)
    let skipSpin = (initialState == Self.contended) && (depth >= maxActiveSpinners)

    if !skipSpin {
      // Cycle-counter low bits provide per-thread jitter to de-correlate pause timings across threads released
      // together by a lock handoff.
      let jitter = UInt32(truncatingIfNeeded: _cycleCounter())
      let mask = pauseBase &- 1
      var spinsRemaining = spinTries

      repeat {
        // Do a relaxed load of the futex value to prevent introducing a memory
        // barrier on each iteration of this loop. We're already informing the
        // CPU that this is a spin loop via the '_spinLoopHint' call which
        // should hopefully slow down the loop a considerable amount to view an
        // actually change in the value potentially. An extra memory barrier
        // would make it even slower on top of the fact that we may not even be
        // able to attempt to acquire the lock.
        var state = storage.load(ordering: .relaxed)

        if state == Self.unlocked {
          let (exchanged, original) = storage.compareExchange(
            expected: Self.unlocked,
            desired: Self.locked,
            successOrdering: .acquiring,
            failureOrdering: .relaxed
          )
          if exchanged {
            // Locked!
            return
          }
          // CAS failed: another thread beat us to `locked`, or a thread in the kernel phase wrote `contended`.
          // Refresh state with what we actually observed so the check below sees it.
          state = original
        }

        // Don't steal the lock from a thread the kernel is about to wake.
        if state == Self.contended { break }

        // Inform the CPU that we're doing a spin loop which should have the
        // effect of slowing down this loop if only by a little to preserve
        // energy.
        let pauses = pauseBase &+ (jitter & mask)
        for _ in 0 ..< pauses {
          _spinLoopHint()
        }

        spinsRemaining -= 1
      } while spinsRemaining > 0
    }

    // We've exhausted our spins (or the depth gate told us to skip them). Ask the kernel to block for us until the
    // owner releases the lock.
    var visibleToSpinners = false

    while true {
      // `.contended`, not `.locked`: parked threads exist and must be woken by the next unlock.
      // Runs unconditionally, even on the skipSpin path: this exchange sometimes grabs the lock
      // (when the unlocker released during the gate->kernel transit), cheaper than forcing every
      // gated thread through a `futex_wait` + wake round-trip.
      if storage.exchange(Self.contended, ordering: .acquiring) == Self.unlocked {
        if visibleToSpinners {
          _ = slowPathDepth.wrappingSubtract(1, ordering: .relaxed)
        }
        // Locked!
        return
      }

      // Didn't get the lock - either it's still held, or we were woken but a spinner / fast-path arrival got in first.
      if !visibleToSpinners {
        // Make arriving threads park instead of spinning, so parked threads can make progress.
        _ = slowPathDepth.wrappingAdd(1, ordering: .relaxed)
        visibleToSpinners = true
      }

      // Sleep while `*word == .contended`. Returns 0 on a normal wake from `FUTEX_WAKE`, or an errno for retryable cases.
      let waitResult = storage._futexWait(expected: Self.contended)
      switch waitResult {
      // `EINTR`  - "A `FUTEX_WAIT` or `FUTEX_WAIT_BITSET` operation was interrupted
      //             by a signal (see signal(7)). Before Linux 2.6.22, this error
      //             could also be returned for a spurious wakeup; since Linux
      //             2.6.22, this no longer happens."
      // `EAGAIN` - "The expected value specified by val did not match the value
      //             in the futex word"; another thread unlocked in the window
      //             between our `exchange(.contended)` and the syscall.
      case 0, 11, 4:
        continue

      default:
        // TODO: Replace with a colder function / one that takes a StaticString
        fatalError("Unknown error occurred while attempting to acquire a Mutex: \(waitResult)")
      }
    }
  }

  @available(SwiftStdlib 6.0, *)
  @_alwaysEmitIntoClient
  @_transparent
  internal borrowing func _tryLock() -> Bool {
    // Do a user space cmpxchg to see if we can easily acquire the lock.
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
    // Release the lock atomically in userspace. Previous value tells us whether anyone is parked.
    if storage.exchange(Self.unlocked, ordering: .releasing) == Self.locked {
      // No waiters, unlocked!
      return
    }

    // At least one waiter parked in the kernel; wake one via `FUTEX_WAKE`.
    _unlockSlow()
  }

  @available(SwiftStdlib 6.0, *)
  @usableFromInline
  internal borrowing func _unlockSlow() {
    // Wake exactly one parked waiter. Remaining parkers and newly-arriving spinners compete on the next release.
    _ = storage._futexWake(count: 1)
  }
}
