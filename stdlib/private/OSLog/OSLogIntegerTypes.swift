//===----------------- OSLogIntegerTypes.swift ----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This file defines extensions for interpolating integer expressions into a
// OSLogMesage. It defines `appendInterpolation` functions for standard integer
// types. It also defines extensions for serializing integer types into the
// argument buffer passed to os_log ABIs.
//
// The `appendInterpolation` functions defined in this file accept formatting
// and privacy options along with the interpolated expression as shown below:
//
//         "\(x, format: .hex, privacy: .private\)"

extension OSLogInterpolation {

  /// Define interpolation for expressions of type Int.
  /// - Parameters:
  ///  - number: the interpolated expression of type Int, which is autoclosured.
  ///  - format: a formatting option available for integer types, defined by the
  ///    enum `IntFormat`.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///    The default is public.
  @_transparent
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int,
    format: IntFormat = .decimal,
    privacy: Privacy = .public
  ) {
    appendInteger(number, format: format, privacy: privacy)
  }

  /// Define interpolation for expressions of type Int32.
  /// - Parameters:
  ///  - number: the interpolated expression of type Int32, which is autoclosured.
  ///  - format: a formatting option available for integer types, defined by the
  ///    enum `IntFormat`.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///    The default is public.
  @_transparent
  @_optimize(none)
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int32,
    format: IntFormat = .decimal,
    privacy: Privacy = .public
  ) {
    appendInteger(number, format: format, privacy: privacy)
  }

  /// Given an integer, create and append a format specifier for the integer to the
  /// format string property. Also, append the integer along with necessary headers
  /// to the OSLogArguments property.
  @_transparent
  @_optimize(none)
  @usableFromInline
  internal mutating func appendInteger<T>(
    _ number: @escaping () -> T,
    format: IntFormat,
    privacy: Privacy
  ) where T: FixedWidthInteger {
    guard argumentCount < maxOSLogArgumentCount else { return }

    let isPrivateArgument = isPrivate(privacy)
    formatString +=
      getIntegerFormatSpecifier(
        T.self,
        format,
        isPrivateArgument)
    addIntHeaders(isPrivateArgument, sizeForEncoding(T.self))

    arguments.append(number)
    argumentCount += 1
  }

  /// Update preamble and append argument headers based on the parameters of
  /// the interpolation.
  @_transparent
  @_optimize(none)
  @usableFromInline
  internal mutating func addIntHeaders(_ isPrivate: Bool, _ byteCount: Int) {
    // Append argument header.
    let argumentHeader = getArgumentHeader(isPrivate: isPrivate, type: .scalar)
    arguments.append(argumentHeader)

    // Append number of bytes needed to serialize the argument.
    arguments.append(UInt8(byteCount))

    // Increment total byte size by the number of bytes needed for this
    // argument, which is the sum of the byte size of the argument and
    // two bytes needed for the headers.
    totalBytesForSerializingArguments += byteCount + 2

    preamble = getUpdatedPreamble(isPrivate: isPrivate, isScalar: true)
  }

  /// Construct an os_log format specifier from the given parameters.
  /// This function must be constant evaluable and all its arguments
  /// must be known at compile time.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getIntegerFormatSpecifier<T>(
    _ integerType: T.Type,
    _ format: IntFormat,
    _ isPrivate: Bool
  ) -> String where T : FixedWidthInteger {
    var formatSpecifier: String = isPrivate ? "%{private}" : "%{public}"

    // Add a length modifier to the specifier.
    // TODO: more length modifiers will be added.
    if (integerType.bitWidth == CLongLong.bitWidth) {
      formatSpecifier += "ll"
    }

    // TODO: more format specifiers will be added.
    switch (format) {
    case .hex:
      formatSpecifier += "x"
    case .octal:
      formatSpecifier += "o"
    default:
      formatSpecifier += integerType.isSigned ? "d" : "u"
    }
    return formatSpecifier
  }
}

extension OSLogArguments {
  /// Append an (autoclosured) interpolated expression of integer type, passed to
  /// `OSLogMessage.appendInterpolation`, to the array of closures tracked
  /// by this instance.
  @usableFromInline
  internal mutating func append<T>(
    _ value: @escaping () -> T
  ) where T: FixedWidthInteger {
    argumentClosures.append({ (position, _) in
      serialize(value(), at: &position)
    })
  }
}

/// Return the number of bytes needed for serializing an integer argument as
/// specified by os_log. This function must be constant evaluable.
@inlinable
@_semantics("constant_evaluable")
@_effects(readonly)
@_optimize(none)
internal func sizeForEncoding<T>(
  _ type: T.Type
) -> Int where T : FixedWidthInteger  {
  return type.bitWidth &>> logBitsPerByte
}

/// Serialize an integer at the buffer location that `position` points to and
/// increment `position` by the byte size of `T`.
@usableFromInline
@_alwaysEmitIntoClient
internal func serialize<T>(
  _ value: T,
  at bufferPosition: inout ByteBufferPointer
) where T : FixedWidthInteger {
  let byteCount = sizeForEncoding(T.self)
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: byteCount)
  withUnsafeBytes(of: value) { dest.copyMemory(from: $0) }
  bufferPosition += byteCount
}
