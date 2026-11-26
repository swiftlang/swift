//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// The clocks of hosted POSIX platforms, read from the C library.
//===----------------------------------------------------------------------===//

#if !os(Windows)

#if !(os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS) || os(Linux) || os(Android) || os(FreeBSD) || os(OpenBSD) || os(WASI) || os(Emscripten) || os(Cygwin) || os(Haiku))
#error("The Swift clocks need an explicit clock branch for this platform")
#endif

import Swift
@_implementationOnly import SwiftConcurrencyInternalShims

@_extern(c, "clock_gettime")
private func _systemClockGetTime(
  _ clock: swift_concurrency_clockid_t,
  _ time: UnsafeMutablePointer<swift_concurrency_timespec>
) -> CInt

@_extern(c, "clock_getres")
private func _systemClockGetResolution(
  _ clock: swift_concurrency_clockid_t,
  _ resolution: UnsafeMutablePointer<swift_concurrency_timespec>
) -> CInt

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
@_extern(c, "clock_gettime_nsec_np")
private func _systemClockGetTimeNanoseconds(
  _ clock: swift_concurrency_clockid_t
) -> UInt64
#endif

@_extern(c, "nanosleep")
private func _systemNanosleep(
  _ request: UnsafePointer<swift_concurrency_timespec>,
  _ remaining: UnsafeMutablePointer<swift_concurrency_timespec>?
) -> CInt


// The system clocks of ClockShims.h. On WASI they are selectors for
// swift_concurrency_systemClock, because a WASI clock ID is a pointer.
private var _continuousSystemClock: CInt { SWIFT_CONCURRENCY_CONTINUOUS_CLOCK }
private var _suspendingSystemClock: CInt { SWIFT_CONCURRENCY_SUSPENDING_CLOCK }
private var _suspendingResolutionSystemClock: CInt {
  SWIFT_CONCURRENCY_SUSPENDING_RESOLUTION_CLOCK
}
private var _wallSystemClock: CInt { SWIFT_CONCURRENCY_WALL_CLOCK }

@unsafe
private func _systemClockID(_ clock: CInt) -> swift_concurrency_clockid_t {
#if os(WASI)
  unsafe swift_concurrency_systemClock(clock)
#else
  swift_concurrency_clockid_t(clock)
#endif
}

/// The runtime clocks on the system clocks of ClockShims.h. Each time and
/// resolution is seconds and nanoseconds.
enum PlatformClock {
  static var continuousTime: (seconds: Int64, nanoseconds: Int64) {
    readTime(_continuousSystemClock)
  }

  static var continuousResolution: (seconds: Int64, nanoseconds: Int64) {
    readResolution(_continuousSystemClock)
  }

  static var suspendingTime: (seconds: Int64, nanoseconds: Int64) {
    readTime(_suspendingSystemClock)
  }

  static var suspendingResolution: (seconds: Int64, nanoseconds: Int64) {
    readResolution(_suspendingResolutionSystemClock)
  }

  static var wallTime: (seconds: Int64, nanoseconds: Int64) {
    readTime(_wallSystemClock)
  }

  static var wallResolution: (seconds: Int64, nanoseconds: Int64) {
    readResolution(_wallSystemClock)
  }

  /// Blocks the thread for the duration, and continues after a signal. An
  /// invalid duration returns at once, so a failed nanosleep means EINTR.
  static func sleep(seconds: Int64, nanoseconds: Int64) {
    var remaining = swift_concurrency_timespec(
      tv_sec: swift_concurrency_time_t(truncatingIfNeeded: seconds),
      tv_nsec: Int(truncatingIfNeeded: nanoseconds))
    guard remaining.tv_sec >= 0,
          remaining.tv_nsec >= 0, remaining.tv_nsec < 1_000_000_000 else {
      return
    }
    withUnsafeMutablePointer(to: &remaining) { remaining in
#if os(WASI)
      // SAFETY: `remaining` is valid in this closure. WASI has no signals, so
      // nanosleep cannot stop early. A WASI runtime can fail it for other
      // reasons, such as a missing host clock, so a retry could never end.
      _ = unsafe _systemNanosleep(remaining, remaining)
#else
      // SAFETY: `remaining` is valid in this closure, and nanosleep permits
      // the same buffer for both arguments.
      while unsafe _systemNanosleep(remaining, remaining) == -1 {
      }
#endif
    }
  }

  /// The time of a system clock. A failed call gives zero.
  private static func readTime(
    _ systemClock: CInt
  ) -> (seconds: Int64, nanoseconds: Int64) {
#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
    // Darwin's clock_gettime reads the same value with clock_gettime_nsec_np
    // and splits it, so this gives the same result with less work.
    let nanoseconds = _systemClockGetTimeNanoseconds(
      swift_concurrency_clockid_t(systemClock))
    return (Int64(truncatingIfNeeded: nanoseconds / 1_000_000_000),
            Int64(truncatingIfNeeded: nanoseconds % 1_000_000_000))
#else
    var value = swift_concurrency_timespec(tv_sec: 0, tv_nsec: 0)
    // SAFETY: `value` is a `struct timespec` for the duration of the call.
    _ = unsafe _systemClockGetTime(_systemClockID(systemClock), &value)
    return (Int64(value.tv_sec), Int64(value.tv_nsec))
#endif
  }

  /// The resolution of a system clock. A failed call gives zero.
  private static func readResolution(
    _ systemClock: CInt
  ) -> (seconds: Int64, nanoseconds: Int64) {
    var value = swift_concurrency_timespec(tv_sec: 0, tv_nsec: 0)
    // SAFETY: `value` is a `struct timespec` for the duration of the call.
    _ = unsafe _systemClockGetResolution(_systemClockID(systemClock), &value)
    return (Int64(value.tv_sec), Int64(value.tv_nsec))
  }
}

#endif
