//===----------------- OSLogFloatingPointTypes.swift ----------------------===//
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

// This file defines extensions for interpolating floating-point expressions
// into an OSLogMessage. It defines `appendInterpolation` functions for standard
// floating-point types. It also defines extensions for serializing floating-
// point types into the argument buffer passed to os_log ABIs.
//
// The `appendInterpolation` functions defined in this file accept privacy
// options along with the interpolated expression as shown below:
//
//    "\(x, format: .fixed(precision: 10), privacy: .private\)"

import ObjectiveC

extension OSLogInterpolation {

  /// Defines interpolation for expressions of type Float.
  ///
  /// Do not call this function directly. It will be called automatically when interpolating
  /// a value of type `Float` in the string interpolations passed to the log APIs.
  ///
  /// - Parameters:
  ///   - number: The interpolated expression of type Float, which is autoclosured.
  ///   - format: A formatting option available for float types, defined by the
  ///     type`OSLogFloatFormatting`. The default is `.fixed`.
  ///   - align: Left or right alignment with the minimum number of columns as
  ///     defined by the type `OSLogStringAlignment`.
  ///   - privacy: A privacy qualifier which is either private or public.
  ///     It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  @_semantics("oslog.requires_constant_arguments")
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Float,
    format: OSLogFloatFormatting = .fixed,
    align: OSLogStringAlignment = .none,
    privacy: OSLogPrivacy = .auto
  ) {
    appendInterpolation(
      Double(number()),
      format: format,
      align: align,
      privacy: privacy)
  }

  /// Define interpolation for expressions of type Double.
  ///
  /// Do not call this function directly. It will be called automatically when interpolating
  /// a value of type `Double` in the string interpolations passed to the log APIs.
  ///
  /// - Parameters:
  ///   - number: The interpolated expression of type Double, which is autoclosured.
  ///   - format: A formatting option available for float types, defined by the
  ///     type`OSLogFloatFormatting`. The default is `.fixed`.
  ///   - align: Left or right alignment with the minimum number of columns as
  ///     defined by the type `OSLogStringAlignment`.
  ///   - privacy: A privacy qualifier which is either private or public.
  ///     It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  @_semantics("oslog.requires_constant_arguments")
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Double,
    format: OSLogFloatFormatting = .fixed,
    align: OSLogStringAlignment = .none,
    privacy: OSLogPrivacy = .auto
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }
    formatString +=
      format.formatSpecifier(for: Double.self, align: align, privacy: privacy)

    // If minimum column width is specified, append this value first. Note that
    // the format specifier would use a '*' for width e.g. %*f.
    if let minColumns = align.minimumColumnWidth {
      appendAlignmentArgument(minColumns)
    }

    // If the privacy has a mask, append the mask argument, which is a constant payload.
    // Note that this should come after the width but before the precision.
    if privacy.hasMask {
      appendMaskArgument(privacy)
    }

    // If minimum number of digits (precision) is specified, append the
    // precision before the argument. Note that the format specifier would use
    // a '*' for precision: %.*f.
    if let precision = format.precision {
      appendPrecisionArgument(precision)
    }
    // Append the double.
    addDoubleHeaders(privacy)
    arguments.append(number)
    argumentCount += 1
  }

  /// Update preamble and append argument headers based on the parameters of
  /// the interpolation.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func addDoubleHeaders(_ privacy: OSLogPrivacy) {
    // Append argument header.
    let argumentHeader = getArgumentHeader(privacy: privacy, type: .scalar)
    arguments.append(argumentHeader)

    // Append number of bytes needed to serialize the argument.
    let byteCount = doubleSizeInBytes()
    arguments.append(UInt8(byteCount))

    // Increment total byte size by the number of bytes needed for this
    // argument, which is the sum of the byte size of the argument and
    // two bytes needed for the headers.
    totalBytesForSerializingArguments += byteCount + 2

    preamble = getUpdatedPreamble(privacy: privacy, isScalar: true)
  }
}

extension OSLogArguments {
  /// Append an (autoclosured) interpolated expression of Double type, passed to
  /// `OSLogMessage.appendInterpolation`, to the array of closures tracked
  /// by this instance.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func append(_ value: @escaping () -> Double) {
    argumentClosures.append({ (position, _, _) in
      serialize(value(), at: &position)
    })
  }
}

/// Return the number of bytes needed for serializing a double argument as
/// specified by os_log. Note that this is marked transparent instead of
/// @inline(__always) as it is used in optimize(none) functions.
@_transparent
@_alwaysEmitIntoClient
internal func doubleSizeInBytes() -> Int {
  return 8
}

/// Serialize a double at the buffer location that `position` points to and
/// increment `position` by the byte size of the double.
@_alwaysEmitIntoClient
@inline(__always)
internal func serialize(
  _ value: Double,
  at bufferPosition: inout ByteBufferPointer
) {
  let byteCount = doubleSizeInBytes()
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: byteCount)
  withUnsafeBytes(of: value) { dest.copyMemory(from: $0) }
  bufferPosition += byteCount
}

