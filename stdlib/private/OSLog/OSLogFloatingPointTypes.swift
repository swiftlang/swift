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
// into an OSLogMesage. It defines `appendInterpolation` functions for standard
// floating-point types. It also defines extensions for serializing floating-
// point types into the argument buffer passed to os_log ABIs.
//
// The `appendInterpolation` functions defined in this file accept privacy
// options along with the interpolated expression as shown below:
// TODO: support floating-point formatting options.
//
//    "\(x, privacy: .private\)"

extension OSLogInterpolation {

  /// Define interpolation for expressions of type Float.
  /// - Parameters:
  ///  - number: the interpolated expression of type Float, which is autoclosured.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///    It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @_semantics("oslog.requires_constant_arguments")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Float,
    privacy: OSLogPrivacy = .auto
  ) {
    appendInterpolation(Double(number()), privacy: privacy)
  }

  /// Define interpolation for expressions of type Double.
  /// - Parameters:
  ///  - number: the interpolated expression of type Double, which is autoclosured.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///    It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @_semantics("oslog.requires_constant_arguments")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Double,
    privacy: OSLogPrivacy = .auto
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    formatString += getDoubleFormatSpecifier(privacy: privacy)
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

  /// Construct an os_log format specifier from the given parameters.
  /// This function must be constant evaluable and all its arguments
  /// must be known at compile time.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getDoubleFormatSpecifier(privacy: OSLogPrivacy) -> String {
    // TODO: this will become more sophisticated when floating-point formatting
    // options are supported.
    var specifier = "%"
    switch privacy {
    case .private:
      specifier += "{private}"
    case .public:
      specifier += "{public}"
    default:
      break
    }
    specifier += "f"
    return specifier
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
    argumentClosures.append({ (position, _) in
      serialize(value(), at: &position)
    })
  }
}

/// Return the number of bytes needed for serializing a double argument as
/// specified by os_log. Note that this is marked transparent instead of
/// @inline(__always) as it is used in optimize(none) functions.
@_transparent
@usableFromInline
internal func doubleSizeInBytes() -> Int {
  return 8
}

/// Serialize a double at the buffer location that `position` points to and
/// increment `position` by the byte size of the double.
@inlinable
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
