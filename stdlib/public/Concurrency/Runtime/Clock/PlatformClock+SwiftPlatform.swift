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
// The clocks of Embedded Swift, read through the platform abstraction layer.
//===----------------------------------------------------------------------===//

import Swift

// The hooks of SwiftPlatform/Clocks.h. They are declared here, not imported
// from the SwiftPlatform module, because clients compile this code from the
// _Concurrency module and not every Swift SDK has that module.
@_extern(c, "_swift_clockContinuous_getTime")
internal func _clockContinuousGetTime(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>)

@_extern(c, "_swift_clockContinuous_getResolution")
internal func _clockContinuousGetResolution(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>)

@_extern(c, "_swift_clockSuspending_getTime")
internal func _clockSuspendingGetTime(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>)

@_extern(c, "_swift_clockSuspending_getResolution")
internal func _clockSuspendingGetResolution(
  _ seconds: UnsafeMutablePointer<Int64>,
  _ nanoseconds: UnsafeMutablePointer<Int64>)

/// The runtime clocks on the hooks of the platform abstraction layer. Each
/// time and resolution is seconds and nanoseconds.
enum PlatformClock {
  static var continuousTime: (seconds: Int64, nanoseconds: Int64) {
    var value: (seconds: Int64, nanoseconds: Int64) = (0, 0)
    // SAFETY: both pointers are valid for the duration of the call.
    unsafe _clockContinuousGetTime(&value.seconds, &value.nanoseconds)
    return value
  }

  static var continuousResolution: (seconds: Int64, nanoseconds: Int64) {
    var value: (seconds: Int64, nanoseconds: Int64) = (0, 0)
    // SAFETY: both pointers are valid for the duration of the call.
    unsafe _clockContinuousGetResolution(
      &value.seconds, &value.nanoseconds)
    return value
  }

  static var suspendingTime: (seconds: Int64, nanoseconds: Int64) {
    var value: (seconds: Int64, nanoseconds: Int64) = (0, 0)
    // SAFETY: both pointers are valid for the duration of the call.
    unsafe _clockSuspendingGetTime(&value.seconds, &value.nanoseconds)
    return value
  }

  static var suspendingResolution: (seconds: Int64, nanoseconds: Int64) {
    var value: (seconds: Int64, nanoseconds: Int64) = (0, 0)
    // SAFETY: both pointers are valid for the duration of the call.
    unsafe _clockSuspendingGetResolution(
      &value.seconds, &value.nanoseconds)
    return value
  }
}
