//===----------------- OSLogMessage.swift ---------------------------------===//
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

// This file contains data structures and helper functions that are used by
// the new OS log APIs.

import ObjectiveC

/// Maximum number of arguments i.e., interpolated expressions that can
/// be used in the string interpolations passed to the log APIs.
/// This limit is imposed by the logging system.
@_semantics("constant_evaluable")
@inlinable
@_optimize(none)
public var maxOSLogArgumentCount: UInt8 { return 48 }

// Note that this is marked transparent instead of @inline(__always) as it is
// used in optimize(none) functions.
@_transparent
@_alwaysEmitIntoClient
internal var logBitsPerByte: Int { return 3 }

/// Represents a string interpolation passed to the log APIs.
///
/// This type converts (through its methods) the given string interpolation into
/// a C-style format string and a sequence of arguments.
///
/// - Warning: Do not explicitly refer to this type. It will be implicitly created
/// by the compiler when you pass a string interpolation to the log APIs.
@frozen
public struct OSLogInterpolation : StringInterpolationProtocol {
  /// A format string constructed from the given string interpolation to be
  /// passed to the os_log ABI.
  @usableFromInline
  internal var formatString: String

  /// A representation of a sequence of arguments that must be serialized
  /// to a byte buffer and passed to the os_log ABI. Each argument, which is
  /// an (autoclosured) expressions that is interpolated, is prepended with a
  /// two byte header. The first header byte consists of a four bit flag and
  /// a four bit type. The second header byte has the size of the argument in
  /// bytes. This is schematically illustrated below.
  ///                 ----------------------------
  ///                 | 4-bit type  | 4-bit flag  |
  ///                 ----------------------------
  ///                 | 1st argument size in bytes|
  ///                 ----------------------------
  ///                 |     1st argument bytes    |
  ///                 ----------------------------
  ///                 | 4-bit type  | 4-bit flag  |
  ///                 -----------------------------
  ///                 | 2nd argument size in bytes|
  ///                 ----------------------------
  ///                 |     2nd argument bytes    |
  ///                 ----------------------------
  ///                         ...
  @usableFromInline
  internal var arguments: OSLogArguments

  /// The possible values for the argument type, as defined by the os_log ABI,
  /// which occupies four most significant bits of the first byte of the
  /// argument header. The rawValue of this enum must be constant evaluable.
  /// (Note that an auto-generated rawValue is not constant evaluable because
  /// it cannot be annotated so.)
  @usableFromInline
  internal enum ArgumentType {
    case scalar, count, string, pointer, object, mask

    @inlinable
    internal var rawValue: UInt8 {
      switch self {
      case .scalar:
        return 0
      case .count:
        return 1
      case .string:
        return 2
      case .pointer:
        return 3
      case .mask:
        return 7
      default: //.object
        return 4
      }
    }
  }

  /// The first summary byte in the byte buffer passed to the os_log ABI that
  /// summarizes the privacy and nature of the arguments.
  @usableFromInline
  internal var preamble: UInt8

  /// Denotes the bit that indicates whether there is private argument.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal var privateBitMask: UInt8 { 0x1 }

  /// Denotes the bit that indicates whether there is non-scalar argument:
  /// String, NSObject or Pointer.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal var nonScalarBitMask: UInt8 { 0x2 }

  /// The second summary byte that denotes the number of arguments, which is
  /// also the number of interpolated expressions. This will be determined
  /// on the fly in order to support concatenation and interpolation of
  /// instances of `OSLogMessage`.
  @usableFromInline
  internal var argumentCount: UInt8

  /// Sum total of all the bytes (including header bytes) needed for
  /// serializing the arguments.
  @usableFromInline
  internal var totalBytesForSerializingArguments: Int

  /// The number of arguments that are Strings. This count is used to create
  /// auxiliary storage meant for extending the lifetime of the string arguments
  /// until the log call completes.
  @usableFromInline
  internal var stringArgumentCount: Int

  /// The number of arguments that are NSObjects. This count is used to create
  /// auxiliary storage meant for extending the lifetime of the NSObject
  /// arguments until the log call completes.
  @usableFromInline
  internal var objectArgumentCount: Int

  // Some methods defined below are marked @_optimize(none) to prevent inlining
  // of string internals (such as String._StringGuts) which will interfere with
  // constant evaluation and folding. Note that these methods will be inlined,
  // constant evaluated/folded and optimized in the context of a caller.

  @_semantics("oslog.interpolation.init")
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public init(literalCapacity: Int, interpolationCount: Int) {
    // Since the format string and the arguments array are fully constructed
    // at compile time, the parameters are ignored.
    formatString = ""
    arguments = OSLogArguments()
    preamble = 0
    argumentCount = 0
    totalBytesForSerializingArguments = 0
    stringArgumentCount = 0
    objectArgumentCount = 0
  }

  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public mutating func appendLiteral(_ literal: String) {
    formatString += literal.percentEscapedString
  }

  /// `appendInterpolation` conformances will be added by extensions to this type.

  /// Compute a byte-sized argument header consisting of flag and type.
  /// Flag and type take up the least and most significant four bits
  /// of the header byte, respectively.
  /// This function should be constant evaluable.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getArgumentHeader(
    privacy: OSLogPrivacy,
    type: ArgumentType
  ) -> UInt8 {
    return (type.rawValue &<< 4) | privacy.argumentFlag
  }

  /// Compute the new preamble based whether the current argument is private
  /// or not. This function must be constant evaluable.
  @inlinable
  @_semantics("constant_evaluable")
  @_effects(readonly)
  @_optimize(none)
  internal func getUpdatedPreamble(
    privacy: OSLogPrivacy,
    isScalar: Bool
  ) -> UInt8 {
    var preamble = self.preamble
    if privacy.isAtleastPrivate {
      preamble |= privateBitMask
    }
    if !isScalar || privacy.hasMask {
      preamble |= nonScalarBitMask
    }
    return preamble
  }
}

extension String {
  /// Replace all percents "%" in the string by "%%" so that the string can be
  /// interpreted as a C format string. This function is constant evaluable
  /// and its semantics is modeled within the evaluator.
  @inlinable
  internal var percentEscapedString: String {
    @_semantics("string.escapePercent.get")
    @_effects(readonly)
    @_optimize(none)
    get {
      return self
        .split(separator: "%", omittingEmptySubsequences: false)
        .joined(separator: "%%")
    }
  }
}

/// Represents a message passed to the log APIs. This type should be created
/// from a string interpolation or a string literal.
///
/// Do not explicitly refer to this type. It will be implicitly created
/// by the compiler when you pass a string interpolation to the log APIs.
@frozen
public struct OSLogMessage :
  ExpressibleByStringInterpolation, ExpressibleByStringLiteral
{
  public let interpolation: OSLogInterpolation

  @inlinable
  @_optimize(none)
  @_semantics("oslog.message.init_interpolation")
  @_semantics("constant_evaluable")
  public init(stringInterpolation: OSLogInterpolation) {
    self.interpolation = stringInterpolation
  }

  @inlinable
  @_optimize(none)
  @_semantics("oslog.message.init_stringliteral")
  @_semantics("constant_evaluable")
  public init(stringLiteral value: String) {
    var s = OSLogInterpolation(literalCapacity: 1, interpolationCount: 0)
    s.appendLiteral(value)
    self.interpolation = s
  }

  /// The byte size of the buffer that will be passed to the logging system.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  public var bufferSize: Int {
    // The two additional bytes is for the preamble and argument count.
    return interpolation.totalBytesForSerializingArguments + 2
  }
}

@usableFromInline
internal typealias ByteBufferPointer = UnsafeMutablePointer<UInt8>
@usableFromInline
internal typealias ObjectStorage<T> = UnsafeMutablePointer<T>?
@usableFromInline
internal typealias ArgumentClosures =
  [(inout ByteBufferPointer,
    inout ObjectStorage<NSObject>,
    inout ObjectStorage<Any>) -> ()]

/// A representation of a sequence of arguments and headers (of possibly
/// different types) that have to be serialized to a byte buffer. The arguments
/// are captured within closures and stored in an array. The closures accept an
/// instance of `OSLogByteBufferBuilder`, and when invoked, serialize the
/// argument using the passed `OSLogByteBufferBuilder` instance.
@frozen
@usableFromInline
internal struct OSLogArguments {
  /// An array of closures that captures arguments of possibly different types.
  /// Each closure accepts a pointer into a byte buffer and serializes the
  /// captured arguments at the pointed location. The closures also accept an
  /// array of AnyObject to store references to auxiliary storage created during
  /// serialization.
  @usableFromInline
  internal var argumentClosures: ArgumentClosures

  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal init() {
    argumentClosures = []
  }

  /// Append a byte-sized header, constructed by
  /// `OSLogMessage.appendInterpolation`, to the tracked array of closures.
  @_semantics("constant_evaluable")
  @inlinable
  @_optimize(none)
  internal mutating func append(_ header: UInt8) {
    argumentClosures.append({ (position, _, _) in
      serialize(header, at: &position)
    })
  }

  /// `append` for other types must be implemented by extensions.
}

/// Serialize a UInt8 value at the buffer location pointed to by `bufferPosition`,
/// and increment the `bufferPosition` with the byte size of the serialized value.
@_alwaysEmitIntoClient
@inline(__always)
internal func serialize(
  _ value: UInt8,
  at bufferPosition: inout ByteBufferPointer)
{
  bufferPosition[0] = value
  bufferPosition += 1
}

// The following code defines helper functions for creating and maintaining
// a buffer for holding a fixed number for instances of a type T. Such buffers
// are used to hold onto NSObjects and Strings that are interpolated in the log
// message until the end of the log call.

@_alwaysEmitIntoClient
@inline(__always)
internal func createStorage<T>(
  capacity: Int,
  type: T.Type
) -> ObjectStorage<T> {
  return
    capacity == 0 ?
      nil :
      UnsafeMutablePointer<T>.allocate(capacity: capacity)
}

@_alwaysEmitIntoClient
@inline(__always)
internal func initializeAndAdvance<T>(
  _ storageOpt: inout ObjectStorage<T>,
  to value: T
) {
  // This if statement should get optimized away.
  if let storage = storageOpt {
    storage.initialize(to: value)
    storageOpt = storage.advanced(by: 1)
  }
}

@_alwaysEmitIntoClient
@inline(__always)
internal func destroyStorage<T>(_ storageOpt: ObjectStorage<T>, count: Int) {
  // This if statement should get optimized away.
  if let storage = storageOpt {
    storage.deinitialize(count: count)
    storage.deallocate()
  }
}
