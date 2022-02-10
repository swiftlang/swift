//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import Swift

@available(SwiftStdlib 5.7, *)
public protocol Clock: Sendable {
  associatedtype Instant: InstantProtocol

  var now: Instant { get }
  var minimumResolution: Instant.Duration { get }

  func sleep(until deadline: Instant, tolerance: Instant.Duration?) async throws
}


@available(SwiftStdlib 5.7, *)
extension Clock {
  @available(SwiftStdlib 5.7, *)
  public func measure(_ work: () throws -> Void) rethrows -> Instant.Duration {
    let start = now
    try work()
    let end = now
    return start.duration(to: end)
  }

  @available(SwiftStdlib 5.7, *)
  public func measure(
    _ work: () async throws -> Void
  ) async rethrows -> Instant.Duration {
    let start = now
    try await work()
    let end = now
    return start.duration(to: end)
  }
}

@available(SwiftStdlib 5.7, *)
@usableFromInline
enum _ClockID: Int32 {
  case continuous = 1
  case suspending = 2
}

@available(SwiftStdlib 5.7, *)
@_silgen_name("swift_get_time")
@usableFromInline
internal func _getTime(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: _ClockID)

@available(SwiftStdlib 5.7, *)
@_silgen_name("swift_get_clock_res")
@usableFromInline
internal func _getClockRes(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: _ClockID)
