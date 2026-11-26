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
// The clocks of Windows.
//===----------------------------------------------------------------------===//

#if os(Windows)

import Swift
@_implementationOnly import SwiftConcurrencyInternalShims

/// The runtime clocks on the Win32 time functions, which count in 100 ns
/// units. Each time and resolution is seconds and nanoseconds.
enum PlatformClock {
  private static let unitsPerSecond: UInt64 = 10_000_000

  static var continuousTime: (seconds: Int64, nanoseconds: Int64) {
    split(interruptTime())
  }

  static var suspendingTime: (seconds: Int64, nanoseconds: Int64) {
    var time: UInt64 = 0
    unsafe QueryUnbiasedInterruptTimePrecise(&time)
    return split(time)
  }

  static var wallTime: (seconds: Int64, nanoseconds: Int64) {
    // FILETIME counts from January 1, 1601. The wall clock counts from
    // January 1, 1970.
    var parts = swift_concurrency_filetime(dwLowDateTime: 0, dwHighDateTime: 0)
    unsafe GetSystemTimePreciseAsFileTime(&parts)
    let fileTime = UInt64(parts.dwHighDateTime) << 32 | UInt64(parts.dwLowDateTime)
    let bias: UInt64 = 11_644_473_600 &* 1_000_000_000
    let nanoseconds = (fileTime &* 100) &- bias
    return (Int64(truncatingIfNeeded: nanoseconds / 1_000_000_000),
            Int64(truncatingIfNeeded: nanoseconds % 1_000_000_000))
  }

  static var continuousResolution: (seconds: Int64, nanoseconds: Int64) {
    (0, 100)
  }

  static var suspendingResolution: (seconds: Int64, nanoseconds: Int64) {
    (0, 100)
  }

  static var wallResolution: (seconds: Int64, nanoseconds: Int64) {
    (0, 100)
  }

  /// Blocks the thread until the interrupt time passes the deadline. SleepEx
  /// can return up to 15 ms early for a sleep of more than 15 ms.
  static func sleep(seconds: Int64, nanoseconds: Int64) {
    var now = interruptTime()
    var delay = UInt64(truncatingIfNeeded:
      seconds &* Int64(unitsPerSecond) &+ nanoseconds / 100)
    let deadline = now &+ delay
    while deadline > now {
      var milliseconds = UInt32(truncatingIfNeeded: delay / 10_000)
      if milliseconds > 15 {
        milliseconds &+= 15
      }
      _ = SleepEx(CUnsignedLong(milliseconds), 1)
      now = interruptTime()
      delay = deadline &- now
    }
  }

  private static func interruptTime() -> UInt64 {
    var time: UInt64 = 0
    unsafe QueryInterruptTimePrecise(&time)
    return time
  }

  private static func split(
    _ time: UInt64
  ) -> (seconds: Int64, nanoseconds: Int64) {
    (Int64(truncatingIfNeeded: time / unitsPerSecond),
     Int64(truncatingIfNeeded: (time % unitsPerSecond) * 100))
  }
}

#endif
