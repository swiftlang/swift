//===----------------- OSLogNSObjectType.swift ----------------------------===//
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

// This file defines extensions for interpolating NSObject into an OSLogMesage.
// It defines `appendInterpolation` function for NSObject type. It also defines
// extensions for generating an os_log format string for NSObjects (using the
// format specifier %@) and for serializing NSObject into the argument buffer
// passed to os_log ABIs.
//
// The `appendInterpolation` function defined in this file accept privacy
// options along with the interpolated expression as shown below:
//
//         "\(x, privacy: .public\)"
import ObjectiveC

extension OSLogInterpolation {

  /// Define interpolation for expressions of type NSObject.
  /// - Parameters:
  ///  - argumentObject: the interpolated expression of type NSObject, which is autoclosured.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///  It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @_semantics("oslog.requires_constant_arguments")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ argumentObject: @autoclosure @escaping () -> NSObject,
    privacy: OSLogPrivacy = .auto
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    formatString += getNSObjectFormatSpecifier(privacy)
    addNSObjectHeaders(privacy)

    arguments.append(argumentObject)
    argumentCount += 1
  }

  /// Update preamble and append argument headers based on the parameters of
  /// the interpolation.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func addNSObjectHeaders(_ privacy: OSLogPrivacy) {
    // Append argument header.
    let header = getArgumentHeader(privacy: privacy, type: .object)
    arguments.append(header)

    // Append number of bytes needed to serialize the argument.
    let byteCount = pointerSizeInBytes()
    arguments.append(UInt8(byteCount))

    // Increment total byte size by the number of bytes needed for this
    // argument, which is the sum of the byte size of the argument and
    // two bytes needed for the headers.
    totalBytesForSerializingArguments += byteCount + 2

    preamble = getUpdatedPreamble(privacy: privacy, isScalar: false)
  }

  /// Construct an os_log format specifier from the given parameters.
  /// This function must be constant evaluable and all its arguments
  /// must be known at compile time.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getNSObjectFormatSpecifier(_ privacy: OSLogPrivacy) -> String {
    switch privacy {
    case .private:
      return "%{private}@"
    case .public:
      return "%{public}@"
    default:
      return "%@"
    }
  }
}

extension OSLogArguments {
  /// Append an (autoclosured) interpolated expression of type NSObject, passed to
  /// `OSLogMessage.appendInterpolation`, to the array of closures tracked
  /// by this instance.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func append(_ value: @escaping () -> NSObject) {
    argumentClosures.append({ (position, _) in
      serialize(value(), at: &position)
    })
  }
}

/// Serialize an NSObject pointer at the buffer location pointed by
/// `bufferPosition`.
@inlinable
@_alwaysEmitIntoClient
@inline(__always)
internal func serialize(
  _ object: NSObject,
  at bufferPosition: inout ByteBufferPointer
) {
  let byteCount = pointerSizeInBytes();
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: byteCount)
  // Get the address of this NSObject as an UnsafeRawPointer.
  let objectAddress = Unmanaged.passUnretained(object).toOpaque()
  // Copy the address into the destination buffer. Note that the input NSObject
  // is an interpolated expression and is guaranteed to be alive until the
  // os_log ABI call is completed by the implementation. Therefore, passing
  // this address to the os_log ABI is safe.
  withUnsafeBytes(of: objectAddress) { dest.copyMemory(from: $0) }
  bufferPosition += byteCount
}
