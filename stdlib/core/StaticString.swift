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
  : ExtendedGraphemeClusterLiteralConvertible,
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
    _BuiltinExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible,
    Printable,
    DebugPrintable {
  var _start: Builtin.RawPointer
  var _byteSize: Builtin.Word
  var _isASCII: Builtin.Int1

  @transparent
  public var start: UnsafePointer<UInt8> {
    return UnsafePointer(_start)
  }

  @transparent
  public var byteSize: UWord {
    return UWord(_byteSize)
  }

  @transparent
  public var isASCII: Bool {
    return Bool(_isASCII)
  }

  @transparent
  public var stringValue: String {
    return String._convertFromBuiltinStringLiteral(
      start.value, byteSize: byteSize.value, isASCII: isASCII.value)
  }

  @transparent
  public init() {
    self = ""
  }

  @transparent
  init(
    start: Builtin.RawPointer, byteSize: Builtin.Word, isASCII: Builtin.Int1
  ) {
    self._start = start
    self._byteSize = byteSize
    self._isASCII = isASCII
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

