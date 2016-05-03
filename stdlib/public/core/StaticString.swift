//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Implementation Note: Because StaticString is used in the
// implementation of _precondition(), _fatalErrorMessage(), etc., we
// keep it extremely close to the bare metal.  In particular, because
// we store only Builtin types, we are guaranteed that no assertions
// are involved in its construction.  This feature is crucial for
// preventing infinite recursion even in non-asserting cases.

/// A simple string designed to represent text that is "knowable at
/// compile-time".
///
/// Logically speaking, each instance looks something like this:
///
///      enum StaticString {
///        case ascii(start: UnsafePointer<UInt8>, count: Int)
///        case utf8(start: UnsafePointer<UInt8>, count: Int)
///        case scalar(UnicodeScalar)
///      }
@_fixed_layout
public struct StaticString
  : _BuiltinUnicodeScalarLiteralConvertible,
    _BuiltinExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible,
    UnicodeScalarLiteralConvertible,
    ExtendedGraphemeClusterLiteralConvertible,
    StringLiteralConvertible,
    CustomStringConvertible,
    CustomDebugStringConvertible,
    CustomReflectable {

  /// Either a pointer to the start of UTF-8 data, represented as an integer,
  /// or an integer representation of a single Unicode scalar.
  internal var _startPtrOrData: Builtin.Word

  /// If `_startPtrOrData` is a pointer, contains the length of the UTF-8 data
  /// in bytes.
  internal var _utf8CodeUnitCount: Builtin.Word

  /// Extra flags:
  ///
  /// - bit 0: set to 0 if `_startPtrOrData` is a pointer, or to 1 if it is a
  ///   Unicode scalar.
  ///
  /// - bit 1: set to 1 if `_startPtrOrData` is a pointer and string data is
  ///   ASCII.
  internal var _flags: Builtin.Int8

  /// A pointer to the beginning of UTF-8 code units.
  ///
  /// - Precondition: `self` stores a pointer to either ASCII or UTF-8 code
  ///   units.
  @_transparent
  public var utf8Start: UnsafePointer<UInt8> {
    _precondition(
      hasPointerRepresentation,
      "StaticString should have pointer representation")
    return UnsafePointer(bitPattern: UInt(_startPtrOrData))!
  }

  /// The stored Unicode scalar value.
  ///
  /// - Precondition: `self` stores a single Unicode scalar value.
  @_transparent
  public var unicodeScalar: UnicodeScalar {
    _precondition(
      !hasPointerRepresentation,
      "StaticString should have Unicode scalar representation")
    return UnicodeScalar(UInt32(UInt(_startPtrOrData)))
  }

  /// If `self` stores a pointer to ASCII or UTF-8 code units, the
  /// length in bytes of that data.
  ///
  /// If `self` stores a single Unicode scalar value, the value of
  /// `utf8CodeUnitCount` is unspecified.
  @_transparent
  public var utf8CodeUnitCount: Int {
    _precondition(
      hasPointerRepresentation,
      "StaticString should have pointer representation")
    return Int(_utf8CodeUnitCount)
  }

  /// `true` iff `self` stores a pointer to ASCII or UTF-8 code units.
  @_transparent
  public var hasPointerRepresentation: Bool {
    return (UInt8(_flags) & 0x1) == 0
  }

  /// `true` if `self` stores a pointer to ASCII code units.
  ///
  /// If `self` stores a single Unicode scalar value, the value of
  /// `isASCII` is unspecified.
  @_transparent
  public var isASCII: Bool {
    return (UInt8(_flags) & 0x2) != 0
  }

  /// Invoke `body` with a buffer containing the UTF-8 code units of
  /// `self`.
  ///
  /// This method works regardless of what `self` stores.
  public func withUTF8Buffer<R>(
    invoke body: @noescape (UnsafeBufferPointer<UInt8>) -> R) -> R {
    if hasPointerRepresentation {
      return body(UnsafeBufferPointer(
        start: utf8Start, count: Int(utf8CodeUnitCount)))
    } else {
      var buffer: UInt64 = 0
      var i = 0
      let sink: (UInt8) -> Void = {
        buffer = buffer | (UInt64($0) << (UInt64(i) * 8))
        i += 1
      }
      UTF8.encode(unicodeScalar, sendingOutputTo: sink)
      return body(UnsafeBufferPointer(
        start: UnsafePointer(Builtin.addressof(&buffer)),
        count: i))
    }
  }

  /// Create an empty instance.
  @_transparent
  public init() {
    self = ""
  }

  @_versioned
  @_transparent
  internal init(
    _start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    // We don't go through UnsafePointer here to make things simpler for alias
    // analysis. A higher-level algorithm may be trying to make sure an
    // unrelated buffer is not accessed or freed.
    self._startPtrOrData = Builtin.ptrtoint_Word(_start)
    self._utf8CodeUnitCount = utf8CodeUnitCount
    self._flags = Bool(isASCII) ? (0x2 as UInt8)._value : (0x0 as UInt8)._value
  }

  @_versioned
  @_transparent
  internal init(
    unicodeScalar: Builtin.Int32
  ) {
    self._startPtrOrData = UInt(UInt32(unicodeScalar))._builtinWordValue
    self._utf8CodeUnitCount = 0._builtinWordValue
    self._flags = UnicodeScalar(_builtinUnicodeScalarLiteral: unicodeScalar).isASCII
      ? (0x3 as UInt8)._value
      : (0x1 as UInt8)._value
  }

  @effects(readonly)
  @_transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = StaticString(unicodeScalar: value)
  }

  /// Create an instance initialized to `value`.
  @effects(readonly)
  @_transparent
  public init(unicodeScalarLiteral value: StaticString) {
    self = value
  }

  @effects(readonly)
  @_transparent
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    self = StaticString(
      _builtinStringLiteral: start,
      utf8CodeUnitCount: utf8CodeUnitCount,
      isASCII: isASCII
    )
  }

  /// Create an instance initialized to `value`.
  @effects(readonly)
  @_transparent
  public init(extendedGraphemeClusterLiteral value: StaticString) {
    self = value
  }

  @effects(readonly)
  @_transparent
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    self = StaticString(
      _start: start,
      utf8CodeUnitCount: utf8CodeUnitCount,
      isASCII: isASCII)
  }

  /// Create an instance initialized to `value`.
  @effects(readonly)
  @_transparent
  public init(stringLiteral value: StaticString) {
    self = value
  }

  /// A textual representation of `self`.
  public var description: String {
    return withUTF8Buffer {
      (buffer) in
      return String._fromWellFormedCodeUnitSequence(UTF8.self, input: buffer)
    }
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return self.description.debugDescription
  }

  public func _getMirror() -> _Mirror {
    return _reflect(self.description)
  }
}

extension StaticString {
  public var customMirror: Mirror {
    return Mirror(reflecting: String(self))
  }
}

extension StaticString {
  @available(*, unavailable, renamed: "utf8CodeUnitCount")
  public var byteSize: Int {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "use the 'String(_:)' initializer")
  public var stringValue: String {
    Builtin.unreachable()
  }
}

