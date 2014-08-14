//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public protocol AssertStringType
  : UnicodeScalarLiteralConvertible,
    ExtendedGraphemeClusterLiteralConvertible,
    StringLiteralConvertible {
  var stringValue: String { get }
}

/// `StaticStringType` refines `AssertStringType` to make it more specific for
/// overload resolution.
public protocol StaticStringType : AssertStringType {}

// Implementation Note: Because StaticString is used in the
// implementation of assert() and fatal(), we keep it extremely close
// to the bare metal.  In particular, because we use only Builtin
// types, we are guaranteed that no assertions are involved in its
// construction.  This feature is crucial for preventing infinite
// recursion even in non-asserting cases.


/// An extremely simple string designed to represent something
/// "statically knowable".
public struct StaticString
  : StaticStringType,
    _BuiltinUnicodeScalarLiteralConvertible,
    _BuiltinExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible,
    Printable,
    DebugPrintable {

  /// Either a pointer to the start of UTF-8 data, or an integer representation
  /// of a single Unicode scalar.
  var _startPtrOrData: Builtin.RawPointer

  /// If `_startPtrOrData` is a pointer, contains the length of the UTF-8 data
  /// in bytes.
  var _byteSize: Builtin.Word

  /// Extra flags:
  ///
  /// - bit 0: set to 0 if `_startPtrOrData` is a pointer, or to 1 if it is a
  ///   Unicode scalar.
  ///
  /// - bit 1: set to 1 if `_startPtrOrData` is a pointer and string data is
  ///   ASCII.
  var _flags: Builtin.Word

  @transparent
  public var utf8Start: UnsafePointer<UInt8> {
    _precondition(
      hasPointerRepresentation,
      "StaticString should have pointer representation")
    return UnsafePointer(_startPtrOrData)
  }

  @transparent
  public var unicodeScalar: UnicodeScalar {
    _precondition(
      !hasPointerRepresentation,
      "StaticString should have Unicode scalar representation")
    return UnicodeScalar(UInt32(unsafeBitCast(_startPtrOrData, UWord.self)))
  }

  @transparent
  public var byteSize: Word {
    return Word(_byteSize)
  }

  @transparent
  public var hasPointerRepresentation: Bool {
    return (UWord(_flags) & 0x1) == 0
  }

  @transparent
  public var isASCII: Bool {
    return (UWord(_flags) & 0x2) == 1
  }

  public func withUTF8Buffer<R>(body: (UnsafeBufferPointer<UInt8>) -> R) -> R {
    if hasPointerRepresentation {
      return body(UnsafeBufferPointer(start: utf8Start, count: Int(byteSize)))
    } else {
      var buffer: UInt64 = 0
      var i = 0
      var sink = SinkOf<UInt8> {
        (byte) in
        buffer = buffer | (UInt64(byte) << (UInt64(i) * 8))
        ++i
      }
      UTF8.encode(unicodeScalar, output: &sink)
      return body(UnsafeBufferPointer(
        start: UnsafePointer(Builtin.addressof(&buffer)),
        count: i))
    }
  }

  @transparent
  public var stringValue: String {
    return withUTF8Buffer {
      (buffer) in
      return String._fromWellFormedCodeUnitSequence(UTF8.self, input: buffer)
    }
  }

  @transparent
  public init() {
    self = ""
  }

  @transparent
  init(
    start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1
  ) {
    self._startPtrOrData = start
    self._byteSize = byteSize
    self._flags = Bool(isASCII) ? 0x2.value : 0.value
  }

  @transparent
  init(
    unicodeScalar: Builtin.Int32
  ) {
    self._startPtrOrData =
      unsafeBitCast(UWord(UInt32(unicodeScalar)), COpaquePointer.self).value
    self._byteSize = 0.value
    self._flags = 0.value
  }

  @effects(readonly)
  @transparent
  public static func _convertFromBuiltinUnicodeScalarLiteral(
    value: Builtin.Int32
  ) -> StaticString {
    return StaticString(unicodeScalar: value)
  }

  @effects(readonly)
  @transparent
  public static func convertFromUnicodeScalarLiteral(
    value: StaticString
  ) -> StaticString {
    return value
  }

  @effects(readonly)
  @transparent
  public static func _convertFromBuiltinExtendedGraphemeClusterLiteral(
    start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1
  ) -> StaticString {
    return _convertFromBuiltinStringLiteral(
      start, byteSize: byteSize, isASCII: isASCII)
  }

  @effects(readonly)
  @transparent
  public static func convertFromExtendedGraphemeClusterLiteral(
    value: StaticString
  ) -> StaticString {
    return value
  }

  @effects(readonly)
  @transparent
  public static func _convertFromBuiltinStringLiteral(
    start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1
  ) -> StaticString {
    return StaticString(start: start, byteSize: byteSize, isASCII: isASCII)
  }

  @effects(readonly)
  @transparent
  public static func convertFromStringLiteral(
    value: StaticString
  ) -> StaticString {
    return value
  }

  public var description: String {
    return self.stringValue
  }

  public var debugDescription: String {
    return self.stringValue.debugDescription
  }
}

/// A String-like type that can be constructed from string interpolation, and
/// is considered less specific than `StaticString` in overload resolution.
public struct AssertString
  : AssertStringType, StringInterpolationConvertible, Printable,
    DebugPrintable {
  public var stringValue: String

  @transparent
  public init() {
    self.stringValue = ""
  }

  @transparent
  public init(_ value: String) {
    self.stringValue = value
  }

  @effects(readonly)
  @transparent
  public static func convertFromUnicodeScalarLiteral(
    value: String
  ) -> AssertString {
    return AssertString(value)
  }

  @effects(readonly)
  @transparent
  public static func convertFromExtendedGraphemeClusterLiteral(
    value: String
  ) -> AssertString {
    return AssertString(value)
  }

  @effects(readonly)
  @transparent
  public static func convertFromStringLiteral(
    value: String
  ) -> AssertString {
    return AssertString(value)
  }

  public static func convertFromStringInterpolation(
    strings: AssertString...
  ) -> AssertString {
    var result = String()
    for str in strings {
      result += str.stringValue
    }
    return AssertString(result)
  }

  @transparent
  public static func convertFromStringInterpolationSegment<T>(
    expr: T
  ) -> AssertString {
    return AssertString(String.convertFromStringInterpolationSegment(expr))
  }

  public var description: String {
    return self.stringValue
  }

  public var debugDescription: String {
    return self.stringValue.debugDescription
  }
}

