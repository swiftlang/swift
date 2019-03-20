//===----------------- OSLog.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===-----------------------------------------------------------------------===//

// This file contains the new swift APIs for OS log that accept string
// interpolations. This is a prototype meant for experimentation and testing.
// Do not use it outside of tests.

@_exported import os

@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
public struct Logger {
  internal let logObject: OSLog

  /// Create a custom OS log object.
  public init(subsystem: String, category: String) {
    logObject = OSLog(subsystem: subsystem, category: category)
  }

  /// Return the default OS log object.
  public init() {
    logObject = OSLog.default
  }

  /// Log a string interpolation at a given level. The level is `default` if
  /// it is not specified.
  public func log(level: OSLogType = .default, _ message: OSLogMessage) {
    osLog(logObject, level, message)
  }

  // TODO: define overloads for logging at specific levels: debug, info, notice,
  // error, fault based on the Swift forum "logging-levels" discussion.
}

/// Given an instance of the custom string interpolation type: `OSLogMessage`,
/// extract the format string, serialize the arguments to a byte buffer,
/// and pass them to the OS logging system.
@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
internal func osLog(
  _ logObject: OSLog,
  _ logLevel: OSLogType,
  _ message: OSLogMessage
) {
  guard logObject.isEnabled(type: logLevel) else { return }

  let bufferSize = message.bufferSize
  let bufferMemory = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
  var builder = OSLogByteBufferBuilder(bufferMemory)

  message.serializeArguments(into: &builder)

  message.formatString.withCString { cFormatString in
    ___os_log_impl(UnsafeMutableRawPointer(mutating: #dsohandle),
                   logObject,
                   logLevel,
                   cFormatString,
                   bufferMemory,
                   UInt32(bufferSize))
  }
  bufferMemory.deallocate()
}
