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
  @usableFromInline
  internal let logObject: OSLog

  /// Create a custom OSLog object for logging.
  public init(subsystem: String, category: String) {
    logObject = OSLog(subsystem: subsystem, category: category)
  }

  /// Use the default OSLog object for logging.
  public init() {
    logObject = OSLog.default
  }

  /// Create a Logger instance from an existing OSLog Object.
  public init(_ logObj: OSLog) {
    logObject = logObj
  }

  // Functions defined below are marked @_optimize(none) to prevent inlining
  // of string internals (such as String._StringGuts) which will interfere with
  // constant evaluation and folding. Note that these functions will be inlined,
  // constant evaluated/folded and optimized in the context of a caller.

  /// Log a string interpolation at a given level. The level is `default` if
  /// it is not specified.
  @_transparent
  @_optimize(none)
  public func log(level: OSLogType = .default, _ message: OSLogMessage) {
    osLog(log: logObject, level: level, message)
  }

  // The following overloads are for logging at specific levels. The levels that
  // are supported are debug (also called trace), info, notice (also called
  // default), error (also called warning), fault (also called critical).

  @_transparent
  @_optimize(none)
  public func trace(_ message: OSLogMessage) {
    osLog(log: logObject, level: .debug, message)
  }

  @_transparent
  @_optimize(none)
  public func debug(_ message: OSLogMessage) {
    osLog(log: logObject, level: .debug, message)
  }

  @_transparent
  @_optimize(none)
  public func info(_ message: OSLogMessage) {
    osLog(log: logObject, level: .info, message)
  }

  @_transparent
  @_optimize(none)
  public func notice(_ message: OSLogMessage) {
    osLog(log: logObject, level: .default, message)
  }

  @_transparent
  @_optimize(none)
  public func warning(_ message: OSLogMessage) {
    osLog(log: logObject, level: .error, message)
  }

  @_transparent
  @_optimize(none)
  public func error(_ message: OSLogMessage) {
    osLog(log: logObject, level: .error, message)
  }

  @_transparent
  @_optimize(none)
  public func critical(_ message: OSLogMessage) {
    osLog(log: logObject, level: .fault, message)
  }

  @_transparent
  @_optimize(none)
  public func fault(_ message: OSLogMessage) {
    osLog(log: logObject, level: .fault, message)
  }
}

/// Given an instance of the custom string interpolation type: `OSLogMessage`,
/// extract the format string, serialize the arguments to a byte buffer,
/// and pass them to the OS logging system.
@available(macOS 10.12, iOS 10.0, watchOS 3.0, tvOS 10.0, *)
@_transparent
@_optimize(none)
public func osLog(
  log logObject: OSLog,
  level logLevel: OSLogType,
  _ message: OSLogMessage
) {
  // Compute static constants first so that they can be folded by
  // OSLogOptimization pass.
  let formatString = message.interpolation.formatString
  let preamble = message.interpolation.preamble
  let argumentCount = message.interpolation.argumentCount
  let bufferSize = message.bufferSize
  let uint32bufferSize = UInt32(bufferSize)
  let argumentClosures = message.interpolation.arguments.argumentClosures

  let formatStringPointer = _getGlobalStringTablePointer(formatString)

  // Code that will execute at runtime.
  guard logObject.isEnabled(type: logLevel) else { return }

  // Allocate a byte buffer to store the arguments. The buffer could be stack
  // allocated as it is local to this function and also its size is a
  // compile-time constant.
  let bufferMemory = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
  // Array of references to auxiliary storage created during serialization of
  // strings. This array can be stack allocated.
  var stringStorageObjects: [AnyObject] = []

  var currentBufferPosition = bufferMemory
  serialize(preamble, at: &currentBufferPosition)
  serialize(argumentCount, at: &currentBufferPosition)
  argumentClosures.forEach { $0(&currentBufferPosition, &stringStorageObjects) }

  ___os_log_impl(UnsafeMutableRawPointer(mutating: #dsohandle),
                 logObject,
                 logLevel,
                 formatStringPointer,
                 bufferMemory,
                 uint32bufferSize)

  // The following operation extends the lifetime of argumentClosures,
  // stringStorageObjects, and also of the objects stored in them till this
  // point. This is necessary because __os_log_impl is passed internal pointers
  // to the objects/strings stored in these arrays.
  _fixLifetime(argumentClosures)
  _fixLifetime(stringStorageObjects)
  bufferMemory.deallocate()
}

/// A test helper that constructs a byte buffer and a format string from an
/// instance of `OSLogMessage` using the same logic as the function `osLog`,
/// and applies a given `assertion` to the constructed format string and
/// byte buffer. This function should be used only in tests.
/// - Parameters:
///   - message: An instance of `OSLogMessage` created from string interpolation
///   - assertion: A closure that takes a format string and a pointer to a
///     byte buffer and asserts a condition.
@_transparent
@_optimize(none)
public // @testable
func _checkFormatStringAndBuffer(
  _ message: OSLogMessage,
  with assertion: (String, UnsafeBufferPointer<UInt8>) -> Void
) {
  // Compute static constants first so that they can be folded by
  // OSLogOptimization pass.
  let formatString = message.interpolation.formatString
  let preamble = message.interpolation.preamble
  let argumentCount = message.interpolation.argumentCount
  let bufferSize = message.bufferSize
  let argumentClosures = message.interpolation.arguments.argumentClosures

  // Code that will execute at runtime.
  let bufferMemory = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
  var stringStorageObjects: [AnyObject] = []

  var currentBufferPosition = bufferMemory
  serialize(preamble, at: &currentBufferPosition)
  serialize(argumentCount, at: &currentBufferPosition)
  argumentClosures.forEach { $0(&currentBufferPosition, &stringStorageObjects) }

  assertion(
    formatString,
    UnsafeBufferPointer(start: UnsafePointer(bufferMemory), count: bufferSize))

  _fixLifetime(argumentClosures)
  _fixLifetime(stringStorageObjects)
  bufferMemory.deallocate()
}
