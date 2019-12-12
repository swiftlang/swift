//===----------------- OSLogStringTypes.swift -----------------------------===//
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

// This file defines extensions for interpolating strings into a OSLogMesage.
// It defines `appendInterpolation` function for String type. It also defines
// extensions for serializing strings into the argument buffer passed to
// os_log ABIs. Note that os_log requires passing a stable pointer to an
// interpolated string. The SPI: `_convertConstStringToUTF8PointerArgument`
// is used to construct a stable pointer to a (dynamic) string.
//
// The `appendInterpolation` function defined in this file accept formatting
// and privacy options along with the interpolated expression as shown below:
//
//         "\(x, privacy: .public\)"
//
// TODO: support formatting options such as left and right padding
// (e.g. %10s, %-10s).

extension OSLogInterpolation {

  /// Define interpolation for expressions of type String.
  /// - Parameters:
  ///  - argumentString: the interpolated expression of type String, which is autoclosured.
  ///  - privacy: a privacy qualifier which is either private or public. Default is private.
  ///  TODO: create a specifier to denote auto-inferred privacy level and make it default.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public mutating func appendInterpolation(
    _ argumentString: @autoclosure @escaping () -> String,
    privacy: Privacy = .private
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    let isPrivateArgument = isPrivate(privacy)
    formatString += getStringFormatSpecifier(isPrivateArgument)
    addStringHeaders(isPrivateArgument)

    arguments.append(argumentString)
    argumentCount += 1
  }

  /// Update preamble and append argument headers based on the parameters of
  /// the interpolation.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func addStringHeaders(_ isPrivate: Bool) {
    // Append argument header.
    let header = getArgumentHeader(isPrivate: isPrivate, type: .string)
    arguments.append(header)

    // Append number of bytes needed to serialize the argument.
    let byteCount = sizeForEncoding()
    arguments.append(UInt8(byteCount))

    // Increment total byte size by the number of bytes needed for this
    // argument, which is the sum of the byte size of the argument and
    // two bytes needed for the headers.
    totalBytesForSerializingArguments += byteCount + 2

    preamble = getUpdatedPreamble(isPrivate: isPrivate, isScalar: false)
  }

  /// Construct an os_log format specifier from the given parameters.
  /// This function must be constant evaluable and all its arguments
  /// must be known at compile time.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getStringFormatSpecifier(_ isPrivate: Bool) -> String {
    // TODO: create a specifier to denote auto-inferred privacy.
    return isPrivate ? "%{private}s" : "%{public}s"
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
/// This function must be constant evaluable.
@_semantics("constant_evaluable")
@inlinable
@_optimize(none)
internal func sizeForEncoding() -> Int {
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

  let byteCount = sizeForEncoding()
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: byteCount)
  withUnsafeBytes(of: bytePointer) { dest.copyMemory(from: $0) }
  bufferPosition += byteCount
}
