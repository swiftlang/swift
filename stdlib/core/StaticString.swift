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

// Implementation Note: Because StaticString is used in the
// implementation of _precondition(), _fatalErrorMessage(), etc., we
// keep it extremely close to the bare metal.  In particular, because
// we store only Builtin types, we are guaranteed that no assertions
// are involved in its construction.  This feature is crucial for
// preventing infinite recursion even in non-asserting cases.

/// An extremely simple string designed to represent something
/// "statically knowable".
public struct StaticString
  : _BuiltinUnicodeScalarLiteralConvertible,
    _BuiltinExtendedGraphemeClusterLiteralConvertible,
    _BuiltinStringLiteralConvertible,
    UnicodeScalarLiteralConvertible,
    ExtendedGraphemeClusterLiteralConvertible,
    StringLiteralConvertible,
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
      unsafeBitCast(UWord(UInt32(unicodeScalar)), COpaquePointer.self)._rawValue
    self._byteSize = 0.value
    self._flags = 0.value
  }

  @effects(readonly)
  @transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = StaticString(unicodeScalar: value)
  }

  /// Create an instance initialized to `value`.
  @effects(readonly)
  @transparent
  public init(unicodeScalarLiteral value: StaticString) {
    self = value
  }

  @effects(readonly)
  @transparent
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    self = StaticString(
      _builtinStringLiteral: start, 
      byteSize: byteSize, 
      isASCII: isASCII
    )
  }

  /// Create an instance initialized to `value`.
  @effects(readonly)
  @transparent
  public init(extendedGraphemeClusterLiteral value: StaticString) {
    self = value
  }

  @effects(readonly)
  @transparent
  public init(
    _builtinStringLiteral start: Builtin.RawPointer,
    byteSize: Builtin.Word, 
    isASCII: Builtin.Int1
  ) {
    self = StaticString(start: start, byteSize: byteSize, isASCII: isASCII)
  }

  /// Create an instance initialized to `value`.
  @effects(readonly)
  @transparent
  public init(stringLiteral value: StaticString) {
    self = value
  }

  /// A textual representation of `self`.
  public var description: String {
    return self.stringValue
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return self.stringValue.debugDescription
  }
}
