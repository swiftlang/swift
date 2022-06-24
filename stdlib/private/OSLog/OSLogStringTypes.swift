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

// This file defines extensions for interpolating strings into an OSLogMessage.
// It defines `appendInterpolation` function for String type. It also defines
// extensions for serializing strings into the argument buffer passed to
// os_log ABIs. Note that os_log requires passing a stable pointer to an
// interpolated string.
//
// The `appendInterpolation` function defined in this file accept privacy and
// alignment options along with the interpolated expression as shown below:
//
//  1.  "\(x, privacy: .private, align: .right\)"
//  2.  "\(x, align: .right(columns: 10)\)"

import ObjectiveC

extension OSLogInterpolation {

  /// Defines interpolation for expressions of type String.
  ///
  /// Do not call this function directly. It will be called automatically when interpolating
  /// a value of type `String` in the string interpolations passed to the log APIs.
  ///
  /// - Parameters:
  ///   - argumentString: The interpolated expression of type String, which is autoclosured.
  ///   - align: Left or right alignment with the minimum number of columns as
  ///     defined by the type `OSLogStringAlignment`.
  ///   - privacy: A privacy qualifier which is either private or public.
  ///     It is auto-inferred by default.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  @_semantics("oslog.requires_constant_arguments")
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
      appendAlignmentArgument(minColumns)
    }

    // If the privacy has a mask, append the mask argument, which is a constant payload.
    // Note that this should come after the width but before the precision.
    if privacy.hasMask {
      appendMaskArgument(privacy)
    }

    // Append the string argument.
    addStringHeaders(privacy)
    arguments.append(argumentString)
    argumentCount += 1
    stringArgumentCount += 1
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
    if let privacySpecifier = privacy.privacySpecifier {
      specifier += "{"
      specifier += privacySpecifier
      specifier += "}"
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
    argumentClosures.append({ (position, _, stringArgumentOwners) in
      serialize(
        value(),
        at: &position,
        storingStringOwnersIn: &stringArgumentOwners)
    })
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
@_alwaysEmitIntoClient
internal func pointerSizeInBytes() -> Int {
  return Int.bitWidth &>> logBitsPerByte
}

/// Serialize a stable pointer to the string `stringValue` at the buffer location
/// pointed to by `bufferPosition`.
@_alwaysEmitIntoClient
@inline(__always)
internal func serialize(
  _ stringValue: String,
  at bufferPosition: inout UnsafeMutablePointer<UInt8>,
  storingStringOwnersIn stringArgumentOwners: inout ObjectStorage<Any>
) {
  let stringPointer =
    getNullTerminatedUTF8Pointer(
      stringValue,
      storingStringOwnersIn: &stringArgumentOwners)

  let byteCount = pointerSizeInBytes()
  let dest =
    UnsafeMutableRawBufferPointer(start: bufferPosition, count: byteCount)
  withUnsafeBytes(of: stringPointer) { dest.copyMemory(from: $0) }
  bufferPosition += byteCount
}

/// Return a pointer that points to a contiguous sequence of null-terminated,
/// UTF8 characters. If necessary, extends the lifetime of `stringValue` by
/// using `stringArgumentOwners`.
@_alwaysEmitIntoClient
@inline(never)
internal func getNullTerminatedUTF8Pointer(
  _ stringValue: String,
  storingStringOwnersIn stringArgumentOwners: inout ObjectStorage<Any>
) -> UnsafeRawPointer {
  let (optStorage, bytePointer, _, _, _):
    (AnyObject?, UnsafeRawPointer, Int, Bool, Bool) =
     stringValue._deconstructUTF8(scratch: nil)
  if let storage = optStorage {
    initializeAndAdvance(&stringArgumentOwners, to: storage)
  } else {
    initializeAndAdvance(&stringArgumentOwners, to: stringValue._guts)
  }
  return bytePointer
}
