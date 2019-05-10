//===----------------- OSLogMessage.swift ---------------------------------===//
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

// This file contains data structures and helper functions that are used by
// the new OS log APIs. These are prototype implementations and should not be
// used outside of tests.

/// Formatting options supported by the logging APIs for logging integers.
/// These can be specified in the string interpolation passed to the log APIs.
/// For Example,
///     log.info("Writing to file with permissions: \(perm, format: .octal)")
///
/// See `OSLogInterpolation.appendInterpolation` definitions for default options
/// for integer types.
public enum IntFormat {
  case decimal
  case hex
  case octal
}

/// Privacy qualifiers for indicating the privacy level of the logged data
/// to the logging system. These can be specified in the string interpolation
/// passed to the log APIs.
/// For Example,
///     log.info("Login request from user id \(userid, privacy: .private)")
///
/// See `OSLogInterpolation.appendInterpolation` definitions for default options
/// for each supported type.
public enum Privacy {
  case `private`
  case `public`
}

/// Maximum number of arguments i.e., interpolated expressions that can
/// be used in the string interpolations passed to the log APIs.
/// This limit is imposed by the ABI of os_log.
public var maxOSLogArgumentCount: Int {
  return 48
}

internal var bitsPerByte: Int {
  return 8
}

/// Represents a string interpolation passed to the log APIs.
///
/// This type converts (through its methods) the given string interpolation into
/// a C-style format string and a sequence of arguments, which is represented
/// by the type `OSLogArguments`.
///
/// Do not create an instance of this type directly. It is used by the compiler
/// when you pass a string interpolation to the log APIs.
/// Extend this type with more `appendInterpolation` overloads to enable
/// interpolating additional types.
public struct OSLogInterpolation : StringInterpolationProtocol {
  /// A format string constructed from the given string interpolation to be
  /// passed to the os_log ABI.
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
  internal var arguments: OSLogArguments

  /// The possible values for the argument flag, as defined by the os_log ABI,
  /// which occupies four least significant bits of the first byte of the
  /// argument header. The first two bits are used to indicate privacy and
  /// the other two are reserved.
  internal enum ArgumentFlag: UInt8 {
    case privateFlag = 0x1
    case publicFlag = 0x2
  }

  /// The possible values for the argument type, as defined by the os_log ABI,
  /// which occupies four most significant bits of the first byte of the
  /// argument header.
  internal enum ArgumentType: UInt8 {
    case scalar = 0
    // TODO: more types will be added here.
  }

  /// The first summary byte in the byte buffer passed to the os_log ABI that
  /// summarizes the privacy and nature of the arguments.
  internal var preamble: UInt8

  /// Bit mask for setting bits in the peamble. The bits denoted by the bit
  /// mask indicate whether there is an argument that is private, and whether
  /// there is an argument that is non-scalar: String, NSObject or Pointer.
  internal enum PreambleBitMask: UInt8 {
    case privateBitMask = 0x1
    case nonScalarBitMask = 0x2
  }

  /// The second summary byte that denotes the number of arguments, which is
  /// also the number of interpolated expressions. This will be determined
  /// on the fly in order to support concatenation and interpolation of
  /// instances of `OSLogMessage`.
  internal var argumentCount: UInt8

  public init(literalCapacity: Int, interpolationCount: Int) {
    // TODO: format string must be fully constructed at compile time.
    // The parameters `literalCapacity` and `interpolationCount` are ignored.
    formatString = ""
    arguments = OSLogArguments()
    preamble = 0
    argumentCount = 0
  }

  public mutating func appendLiteral(_ literal: String) {
    formatString += literal.percentEscapedString
  }

  /// Define interpolation for expressions of type Int. This definition enables
  /// passing a formatting option and a privacy qualifier along with the
  /// interpolated expression as shown below:
  ///
  ///         "\(x, format: .hex, privacy: .private\)"
  ///
  /// - Parameters:
  ///  - number: the interpolated expression of type Int, which is autoclosured.
  ///  - format: a formatting option available for Int types, defined by the
  ///    enum `IntFormat`.
  ///  - privacy: a privacy qualifier which is either private or public.
  ///    The default is public.
  public mutating func appendInterpolation(
    _ number: @autoclosure @escaping () -> Int,
    format: IntFormat = .decimal,
    privacy: Privacy = .public
  ) {
    guard argumentCount < maxOSLogArgumentCount else { return }

    addIntHeadersAndFormatSpecifier(
      format,
      isPrivate: privacy == .private,
      bitWidth: Int.bitWidth,
      isSigned: true)
    arguments.append(number)
  }

  /// Construct/update format string and headers from the qualifiers (of the
  /// interpolated expression) passed as parameters.
  ///
  /// All arguments to this function must be known at compile time.
  public mutating func addIntHeadersAndFormatSpecifier(
    _ format: IntFormat,
    isPrivate: Bool,
    bitWidth: Int,
    isSigned: Bool
  ) {
    formatString += getIntegerFormatSpecifier(
      format,
      isPrivate: isPrivate,
      bitWidth: bitWidth,
      isSigned: isSigned)

    addArgumentHeaders(
      flag: isPrivate ? .privateFlag : .publicFlag,
      type: .scalar,
      size: UInt8(bitWidth / bitsPerByte))

    updateSummaryBytes(isPrivate: isPrivate)
  }

  /// Set the private bit of the preamble if the `isPrivate` parameter is true
  /// and increment the argument count. Note that the private bit in the
  /// preamable is set if any of the arguments is private.
  internal mutating func updateSummaryBytes(isPrivate: Bool) {
    if (isPrivate) {
      preamble |= PreambleBitMask.privateBitMask.rawValue
    }
    argumentCount += 1
  }

  /// Append the given argument headers and size.
  internal mutating func addArgumentHeaders(
    flag: ArgumentFlag,
    type: ArgumentType,
    size: UInt8
  ) {
    // Flag and type take up one byte where the least significant four bits
    // is flag and most significant four bits is the type.
    let flagAndType: UInt8 = (type.rawValue << 4) | flag.rawValue
    arguments.append(flagAndType)
    arguments.append(size)
  }

  /// Construct an os_log format specifier from the given parameters.
  /// All arguments to this function must be known at compile time.
  internal func getIntegerFormatSpecifier(
    _ format: IntFormat,
    isPrivate: Bool,
    bitWidth: Int,
    isSigned: Bool
  ) -> String {
    var formatSpecifier: String = isPrivate ? "%{private}" : "%{public}"

    // Add a length modifier, if needed, to the specifier
    // TODO: more length modifiers will be added.
    if (bitWidth == CLongLong.bitWidth) {
      formatSpecifier += "ll"
    }

    // TODO: more format specifiers will be added.
    switch (format) {
    case .hex:
      formatSpecifier += "x"
    case .octal:
      formatSpecifier += "o"
    default:
      formatSpecifier += isSigned ? "d" : "u"
    }
    return formatSpecifier
  }
}

extension String {
  /// Replace all percents "%" in the string by "%%" so that the string can be
  /// interpreted as a C format string.
  public var percentEscapedString: String {
    get {
      return self
        .split(separator: "%", omittingEmptySubsequences: false)
        .joined(separator: "%%")
    }
  }
}

public struct OSLogMessage :
  ExpressibleByStringInterpolation, ExpressibleByStringLiteral
{
  public let interpolation: OSLogInterpolation

  /// Initializer for accepting string interpolations.
  public init(stringInterpolation: OSLogInterpolation) {
    interpolation = stringInterpolation
  }

  /// Initializer for accepting string literals.
  public init(stringLiteral value: String) {
    // Note that the actual value of `literalCapacity` is not important as it
    // is ignored by `OSLogInterpolation.init`. However, it must be a literal.
    var s = OSLogInterpolation(literalCapacity: 1, interpolationCount: 0)
    s.appendLiteral(value)
    self.init(stringInterpolation: s)
  }

  /// Format string constructed from the string interpolation.
  public var formatString: String {
    get { return interpolation.formatString }
  }

  /// The byte size of the buffer that will passed to the C os_log ABI.
  /// It will contain the elements of interpolation.arguments and the two
  /// summary bytes: preamble and argument count.
  public var bufferSize: Int {
    get { return interpolation.arguments.byteCount + 2 }
  }

  /// Serialize the summary bytes and arguments into the given byte-buffer
  /// builder. The summary bytes are serailized first followed by the arguments.
  internal func serializeArguments(
    into bufferBuilder: inout OSLogByteBufferBuilder
  ) {
    bufferBuilder.serialize(interpolation.preamble)
    bufferBuilder.serialize(interpolation.argumentCount)
    interpolation.arguments.serialize(into: &bufferBuilder)
  }
}

/// A representation of a sequence of arguments and headers (of possibly
/// different types) that have to be serialized to a byte buffer. The arguments
/// are captured within closures and stored in an array. The closures accept an
/// instance of `OSLogByteBufferBuilder`, and when invoked, serialize the
/// argument using the passed `OSLogByteBufferBuilder` instance.
internal struct OSLogArguments {
  /// An array of closures that captures arguments of possibly different types.
  internal var argumentClosures: [(inout OSLogByteBufferBuilder) -> ()]
  /// Sum total of the byte size of the arguments that are tracked.
  internal var byteCount: Int

  internal init() {
    argumentClosures = []
    byteCount = 0
  }

  /// Append a byte-sized header, constructed by
  /// `OSLogMessage.appendInterpolation`, to the tracked array of closures.
  internal mutating func append(_ header: UInt8) {
    argumentClosures.append({ $0.serialize(header) })
    byteCount += OSLogByteBufferBuilder.sizeForEncoding(UInt8.self)
  }

  /// Append an (autoclosured) interpolated expression of type Int, passed to
  /// `OSLogMessage.appendInterpolation`, to the tracked array of closures.
  internal mutating func append(_ value: @escaping () -> Int) {
    argumentClosures.append({ $0.serialize(value()) })
    byteCount += OSLogByteBufferBuilder.sizeForEncoding(Int.self)
  }

  internal func serialize(into bufferBuilder: inout OSLogByteBufferBuilder) {
    argumentClosures.forEach { $0(&bufferBuilder) }
  }
}

/// A struct that manages serialization of instances of specific types to a
/// byte buffer. The byte buffer is provided as an argument to the initializer
/// so that its lifetime can be managed by the caller.
internal struct OSLogByteBufferBuilder {
  internal var position: UnsafeMutablePointer<UInt8>

  /// Initializer that accepts a pointer to a preexisting buffer.
  /// - Parameter bufferStart: the starting pointer to a byte buffer
  ///   that must contain the serialized bytes.
  internal init(_ bufferStart: UnsafeMutablePointer<UInt8>) {
    position = bufferStart
  }

  /// Serialize a UInt8 value at the buffer location pointed to by `position`.
  internal mutating func serialize(_ value: UInt8) {
    position[0] = value
    position += 1
  }

  /// Serialize an Int at the buffer location pointed to by `position`.
  internal mutating func serialize(_ value: Int) {
    let byteCount = OSLogByteBufferBuilder.sizeForEncoding(Int.self)
    let dest = UnsafeMutableRawBufferPointer(start: position, count: byteCount)
    withUnsafeBytes(of: value) { dest.copyMemory(from: $0) }
    position += byteCount
  }

  /// Return the number of bytes needed for serializing an UInt8 value.
  internal static func sizeForEncoding(_ type: UInt8.Type) -> Int {
    return 1
  }

  /// Return the number of bytes needed for serializing an Int value.
  internal static func sizeForEncoding(_ type: Int.Type) -> Int {
    return Int.bitWidth / bitsPerByte
  }
}
