//===----------------- OSLogStringTypes.swift -----------------------------===//
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

// This file defines extensions for interpolating strings into an OSLogMesage.
// It defines `appendInterpolation` function for String type. It also defines
// extensions for serializing strings into the argument buffer passed to
// os_log ABIs. Note that os_log requires passing a stable pointer to an
// interpolated string. The SPI: `_convertConstStringToUTF8PointerArgument`
// is used to construct a stable pointer to a (dynamic) string.
//
// The `appendInterpolation` function defined in this file accept privacy and
// alignment options along with the interpolated expression as shown below:
//
//  1.  "\(x, privacy: .private, align: .right\)"
//  2.  "\(x, align: .right(columns: 10)\)"

extension OSLogInterpolation {

  /// Define interpolation for expressions of type String.
  /// - Parameters:
  ///  - argumentString: the interpolated expression of type String, which is autoclosured.
  ///  - align: left or right alignment with the minimum number of columns as
  ///    defined by the type `OSLogStringAlignment`.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///  It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @_semantics("oslog.requires_constant_arguments")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ argumentString: @autoclosure @escaping () -> String,
    align: OSLogStringAlignment = .none,
    privacy: OSLogPrivacy = .auto
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    formatString += getStringFormatSpecifier(align, privacy)

    // If minimum column width is specified, append this value first. Note that the
    // format specifier would use a '*' for width e.g. %*s.
    if let minColumns = align.minimumColumnWidth {
      appendPrecisionArgument(minColumns)
    }

    addStringHeaders(privacy)
    arguments.append(argumentString)
    argumentCount += 1
  }

  /// Update preamble and append argument headers based on the parameters of
  /// the interpolation.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func addStringHeaders(_ privacy: OSLogPrivacy) {
    // Append argument header.
    let header = getArgumentHeader(privacy: privacy, type: .string)
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
  internal func getStringFormatSpecifier(
    _ align: OSLogStringAlignment,
    _ privacy: OSLogPrivacy
  ) -> String {
    var specifier = "%"
    switch privacy {
    case .private:
      specifier += "{private}"
    case .public:
      specifier += "{public}"
    default:
      break
    }
    if case .start = align.anchor {
      specifier += "-"
    }
    if let _ = align.minimumColumnWidth {
      specifier += "*"
    }
    specifier += "s"
    return specifier
  }
}

extension OSLogArguments {
  /// Append an (autoclosured) interpolated expression of String type, passed to
  /// `OSLogMessage.appendInterpolation`, to the array of closures tracked
  /// by this instance.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func append(_ value: @escaping () -> String) {
    argumentClosures.append({ serialize(value(), at: &$0, using: &$1) })
  }
}

/// Return the byte size of a pointer as strings are passed to the C os_log ABIs by
/// a stable pointer to its UTF8 bytes. Since pointers do not have a public
/// bitWidth property, and since MemoryLayout is not supported by the constant
/// evaluator, this function returns the byte size of Int, which must equal the
/// word length of the target architecture and hence the pointer size.
/// This function must be constant evaluable. Note that it is marked transparent
/// instead of @inline(__always) as it is used in optimize(none) functions.
@_transparent
@usableFromInline
internal func pointerSizeInBytes() -> Int {
  return Int.bitWidth &>> logBitsPerByte
}

/// Serialize a stable pointer to the string `stringValue` at the buffer location
/// pointed by `bufferPosition`. When necessary, this function would copy the
/// string contents to a storage with a stable pointer. If that happens, a reference
/// to the storage will be added to `storageObjects`.
@inlinable
@_alwaysEmitIntoClient
@inline(__always)
internal func serialize(
  _ stringValue: String,
  at bufferPosition: inout ByteBufferPointer,
  using storageObjects: inout StorageObjects
) {
  let (optionalStorage, bytePointer): (AnyObject?, UnsafeRawPointer) =
    _convertConstStringToUTF8PointerArgument(
      stringValue)

  if let storage = optionalStorage {
    storageObjects.append(storage)
  }

  let byteCount = pointerSizeInBytes()
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: byteCount)
  withUnsafeBytes(of: bytePointer) { dest.copyMemory(from: $0) }
  bufferPosition += byteCount
}
