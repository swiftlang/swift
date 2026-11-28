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
// The exported clock entry points, for binaries and libraries that call the C
// existing names. The Swift clock code in this module calls PlatformClock
// directly.
//===----------------------------------------------------------------------===//

import Swift

@_extern(c, "swift_reportError")
private func _swiftReportError(_ flags: UInt32, _ message: UnsafePointer<CChar>)

@_extern(c, "abort")
private func _abort()

/// Writes the error to the standard error and the crash log, and aborts.
private func _reportInvalidClockID(_ clock: CInt) -> Never {
  "Fatal error: invalid clock ID \(clock)\n".withCString { message in
    // SAFETY: `withCString` gives a valid C string for the closure.
    unsafe _swiftReportError(0, message)
  }
  _abort()
  Builtin.unreachable()
}

extension PlatformClock {
  /// The time of a runtime clock.
  static func time(of clock: _ClockID) -> (seconds: Int64, nanoseconds: Int64) {
    switch clock {
    case .continuous: continuousTime
    case .suspending: suspendingTime
    case .walltime: wallTime
    }
  }

  /// The resolution of a runtime clock.
  static func resolution(
    of clock: _ClockID
  ) -> (seconds: Int64, nanoseconds: Int64) {
    switch clock {
    case .continuous: continuousResolution
    case .suspending: suspendingResolution
    case .walltime: wallResolution
    }
  }
}

/// `swift_get_time`: the time of `clock` since an arbitrary fixed point.
/// Stops the process for an unknown clock ID.
@available(StdlibDeploymentTarget 5.7, *)
@_silgen_name("swift_get_time")
@usableFromInline
internal func _swiftGetTime(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: CInt
) {
  guard let id = _ClockID(rawValue: clock) else {
    _reportInvalidClockID(clock)
  }
  let time = PlatformClock.time(of: id)
  // SAFETY: the callers of swift_get_time give two valid out-parameters.
  unsafe seconds.pointee = time.seconds
  unsafe nanoseconds.pointee = time.nanoseconds
}

/// `swift_get_clock_res`: the resolution of `clock`.
/// Stops the process for an unknown clock ID.
@available(StdlibDeploymentTarget 5.7, *)
@_silgen_name("swift_get_clock_res")
@usableFromInline
internal func _swiftGetClockRes(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: CInt
) {
  guard let id = _ClockID(rawValue: clock) else {
    _reportInvalidClockID(clock)
  }
  let resolution = PlatformClock.resolution(of: id)
  // SAFETY: the callers of swift_get_clock_res give two valid out-parameters.
  unsafe seconds.pointee = resolution.seconds
  unsafe nanoseconds.pointee = resolution.nanoseconds
}

/// `swift_sleep`: blocks the calling thread for the duration.
@available(StdlibDeploymentTarget 6.3, *)
@_silgen_name("swift_sleep")
@usableFromInline
internal func _swiftSleep(seconds: Int64, nanoseconds: Int64) {
  PlatformClock.sleep(seconds: seconds, nanoseconds: nanoseconds)
}
