//===----------------- OSLogTestHelper.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import ObjectiveC

// This file contains test helpers for testing the compiler diagnostics and optimizations
// of the new swift APIs for os log that accept string interpolations.

// Some functions defined in this file are marked @_optimize(none) to prevent inlining
// of string internals (such as String._StringGuts) which will interfere with
// constant evaluation and folding. Note that these functions will be inlined,
// constant evaluated, folded and optimized in the context of a caller. TODO:
// @_optimize(none) can be removed if (non-mandatory) inlining optimizations can be moved
// after serialization.

/// A function that acts like a check for whether logging is enabled in `_osLogTestHelper`.
@inline(never)
@usableFromInline
internal func isLoggingEnabled() -> Bool { true }

/// A closure that does nothing. Meant to be used as the default assertion of
/// `_osLogTestHelper`.
public let _noopClosure = { (x : String, y : UnsafeBufferPointer<UInt8>) in return }

/// A test helper that constructs a byte buffer and a format string from an
/// instance of `OSLogMessage` using the same logic as the new os log APIs,
/// and applies a given `assertion` to the constructed format string and
/// byte buffer. This function should be used only in tests.
/// - Parameters:
///   - message: An instance of `OSLogMessage` created from string interpolation
///   - assertion: A closure that takes a format string and a pointer to a
///     byte buffer and asserts a condition.
@_semantics("oslog.requires_constant_arguments")
@_transparent
@_optimize(none)
public // @testable
func _osLogTestHelper(
  _ message: OSLogMessage,
  assertion: (String, UnsafeBufferPointer<UInt8>) -> Void = _noopClosure
) {
  // Compute static constants first so that they can be folded by
  // OSLogOptimization pass.
  let formatString = message.interpolation.formatString
  let preamble = message.interpolation.preamble
  let argumentCount = message.interpolation.argumentCount
  let bufferSize = message.bufferSize
  let objectCount = message.interpolation.objectArgumentCount
  let stringCount = message.interpolation.stringArgumentCount
  let uint32bufferSize = UInt32(bufferSize)
  let argumentClosures = message.interpolation.arguments.argumentClosures

  let formatStringPointer = _getGlobalStringTablePointer(formatString)

  // Code that will execute at runtime.
  if (!isLoggingEnabled()) {
    return
  }
  let bufferMemory = UnsafeMutablePointer<UInt8>.allocate(capacity: bufferSize)
  // Buffer for storing NSObjects and strings to keep them alive until the
  // _os_log_impl_test call completes.
  let objectArguments = createStorage(capacity: objectCount, type: NSObject.self)
  let stringArgumentOwners = createStorage(capacity: stringCount, type: Any.self)

  var currentBufferPosition = bufferMemory
  var objectArgumentsPosition = objectArguments
  var stringArgumentOwnersPosition = stringArgumentOwners
  serialize(preamble, at: &currentBufferPosition)
  serialize(argumentCount, at: &currentBufferPosition)
  argumentClosures.forEach {
    $0(&currentBufferPosition,
       &objectArgumentsPosition,
       &stringArgumentOwnersPosition)
  }

  _os_log_impl_test(
    assertion,
    formatString,
    formatStringPointer,
    bufferMemory,
    uint32bufferSize)

  // The following operation extends the lifetime of objectArguments and
  // stringArgumentOwners till this point. This is necessary because the
  // assertion is passed internal pointers to the objects/strings stored
  // in these arrays, as in the actual os log implementation.
  destroyStorage(objectArguments, count: objectCount)
  destroyStorage(stringArgumentOwners, count: stringCount)
  bufferMemory.deallocate()
}

/// A function that pretends to be _os_log_impl.
@inline(never)
@usableFromInline
internal func _os_log_impl_test(
  _ assertion: (String, UnsafeBufferPointer<UInt8>) -> Void,
  _ formatString: String,
  _ formatStringPointer: UnsafePointer<CChar>,
  _ bufferMemory: UnsafeMutablePointer<UInt8>,
  _ bufferSize: UInt32
) {
  assertion(
    formatString,
    UnsafeBufferPointer(
      start: UnsafePointer(bufferMemory),
      count: Int(bufferSize)))
}



/// A function that pretends to be os_signpost(.animationBegin, ...). The purpose
/// of this function is to test whether the OSLogOptimization pass works properly
/// on the special case of animation begin signposts.
@_transparent
public func _osSignpostAnimationBeginTestHelper(
  _ format: AnimationFormatString.MyLogMessage,
  _ arguments: CVarArg...
) {
  _animationBeginSignpostHelper(formatStringPointer: format.formatStringPointer,
                                arguments: arguments)
}

@usableFromInline
internal func _animationBeginSignpostHelper(
  formatStringPointer: UnsafePointer<CChar>,
  arguments: [CVarArg]
) {}

// A namespace for utilities specific to os_signpost animation tests.
public enum AnimationFormatString {
  @inlinable
  @_optimize(none)
  @_semantics("constant_evaluable")
  internal static func constructOSLogInterpolation(
    _ formatString: String
  ) -> OSLogInterpolation {
    var s = OSLogInterpolation(literalCapacity: 1, interpolationCount: 0)
    s.formatString += formatString
    s.formatString += " isAnimation=YES"
    return s
  }

  @frozen
  @_semantics("oslog.message.type")
  public struct MyLogMessage : ExpressibleByStringLiteral {
    @usableFromInline
    var formatStringPointer: UnsafePointer<CChar>

    @_transparent
    public init(stringLiteral value: String) {
      let message =
        OSLogTestHelper.OSLogMessage(
          stringInterpolation:
            constructOSLogInterpolation(
              value))
      let formatString = message.interpolation.formatString
      formatStringPointer = _getGlobalStringTablePointer(formatString)
    }
  }
}
