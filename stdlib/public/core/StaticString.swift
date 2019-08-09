//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Implementation Note: Because StaticString is used in the
// implementation of _precondition(), _fatalErrorMessage(), etc., we
// keep it extremely close to the bare metal.  In particular, because
// we store only Builtin types, we are guaranteed that no assertions
// are involved in its construction.  This feature is crucial for
// preventing infinite recursion even in non-asserting cases.

/// A string type designed to represent text that is known at compile time.
///
/// Instances of the `StaticString` type are immutable. `StaticString` provides
/// limited, pointer-based access to its contents, unlike Swift's more
/// commonly used `String` type. A static string can store its value as a
/// pointer to an ASCII code unit sequence, as a pointer to a UTF-8 code unit
/// sequence, or as a single Unicode scalar value.
@frozen
public struct StaticString
  : _ExpressibleByBuiltinUnicodeScalarLiteral,
    _ExpressibleByBuiltinExtendedGraphemeClusterLiteral,
    _ExpressibleByBuiltinStringLiteral,
    ExpressibleByUnicodeScalarLiteral,
    ExpressibleByExtendedGraphemeClusterLiteral,
    ExpressibleByStringLiteral,
    CustomStringConvertible,
    CustomDebugStringConvertible,
    CustomReflectable {

  /// Either a pointer to the start of UTF-8 data, represented as an integer,
  /// or an integer representation of a single Unicode scalar.
  @usableFromInline
  internal var _startPtrOrData: Builtin.Word

  /// If `_startPtrOrData` is a pointer, contains the length of the UTF-8 data
  /// in bytes.
  @usableFromInline
  internal var _utf8CodeUnitCount: Builtin.Word

  /// Extra flags:
  ///
  /// - bit 0: set to 0 if `_startPtrOrData` is a pointer, or to 1 if it is a
  ///   Unicode scalar.
  ///
  /// - bit 1: set to 1 if `_startPtrOrData` is a pointer and string data is
  ///   ASCII.
  @usableFromInline
  internal var _flags: Builtin.Int8

  /// A pointer to the beginning of the string's UTF-8 encoded representation.
  ///
  /// The static string must store a pointer to either ASCII or UTF-8 code
  /// units. Accessing this property when `hasPointerRepresentation` is
  /// `false` triggers a runtime error.
  @_transparent
  public var utf8Start: UnsafePointer<UInt8> {
    _precondition(
      hasPointerRepresentation,
      "StaticString should have pointer representation")
    return UnsafePointer(bitPattern: UInt(_startPtrOrData))!
  }

  /// The stored Unicode scalar value.
  ///
  /// The static string must store a single Unicode scalar value. Accessing
  /// this property when `hasPointerRepresentation` is `true` triggers a
  /// runtime error.
  @_transparent
  public var unicodeScalar: Unicode.Scalar {
    _precondition(
      !hasPointerRepresentation,
      "StaticString should have Unicode scalar representation")
    return Unicode.Scalar(UInt32(UInt(_startPtrOrData)))!
  }

  /// The length in bytes of the static string's ASCII or UTF-8 representation.
  ///
  /// - Warning: If the static string stores a single Unicode scalar value, the
  ///   value of `utf8CodeUnitCount` is unspecified.
  @_transparent
  public var utf8CodeUnitCount: Int {
    _precondition(
      hasPointerRepresentation,
      "StaticString should have pointer representation")
    return Int(_utf8CodeUnitCount)
  }

  @_alwaysEmitIntoClient @_transparent
  internal var unsafeRawPointer: Builtin.RawPointer {
    return Builtin.inttoptr_Word(_startPtrOrData)
  }

  /// A Boolean value indicating whether the static string stores a pointer to
  /// ASCII or UTF-8 code units.
  @_transparent
  public var hasPointerRepresentation: Bool {
    return (UInt8(_flags) & 0x1) == 0
  }

  /// A Boolean value that is `true` if the static string stores a pointer to
  /// ASCII code units.
  ///
  /// Use this property in conjunction with `hasPointerRepresentation` to
  /// determine whether a static string with pointer representation stores an
  /// ASCII or UTF-8 code unit sequence.
  ///
  /// - Warning: If the static string stores a single Unicode scalar value, the
  ///   value of `isASCII` is unspecified.
  @_transparent
  public var isASCII: Bool {
    return (UInt8(_flags) & 0x2) != 0
  }

  /// Invokes the given closure with a buffer containing the static string's
  /// UTF-8 code unit sequence.
  ///
  /// This method works regardless of whether the static string stores a
  /// pointer or a single Unicode scalar value.
  ///
  /// The pointer argument to `body` is valid only during the execution of
  /// `withUTF8Buffer(_:)`. Do not store or return the pointer for later use.
  ///
  /// - Parameter body: A closure that takes a buffer pointer to the static
  ///   string's UTF-8 code unit sequence as its sole argument. If the closure
  ///   has a return value, that value is also used as the return value of the
  ///   `withUTF8Buffer(invoke:)` method. The pointer argument is valid only
  ///   for the duration of the method's execution.
  /// - Returns: The return value, if any, of the `body` closure.
  @_transparent
  public func withUTF8Buffer<R>(
    _ body: (UnsafeBufferPointer<UInt8>) -> R) -> R {
    if hasPointerRepresentation {
      return body(UnsafeBufferPointer(
        start: utf8Start, count: utf8CodeUnitCount))
    } else {
      var buffer: UInt64 = 0
      var i = 0
      let sink: (UInt8) -> Void = {
#if _endian(little)
        buffer = buffer | (UInt64($0) << (UInt64(i) * 8))
#else
        buffer = buffer | (UInt64($0) << (UInt64(7-i) * 8))
#endif
        i += 1
      }
      UTF8.encode(unicodeScalar, into: sink)
      return body(UnsafeBufferPointer(
        start: UnsafePointer(Builtin.addressof(&buffer)),
        count: i))
    }
  }

  /// Creates an empty static string.
  @_transparent
  public init() {
    self = ""
  }

  @usableFromInline @_transparent
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
    self._flags = Bool(isASCII)
      ? (0x2 as UInt8)._value
      : (0x0 as UInt8)._value
  }

  @usableFromInline @_transparent
  internal init(
    unicodeScalar: Builtin.Int32
  ) {
    self._startPtrOrData = UInt(UInt32(unicodeScalar))._builtinWordValue
    self._utf8CodeUnitCount = 0._builtinWordValue
    self._flags = Unicode.Scalar(_builtinUnicodeScalarLiteral: unicodeScalar).isASCII
      ? (0x3 as UInt8)._value
      : (0x1 as UInt8)._value
  }

  @_effects(readonly)
  @_transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = StaticString(unicodeScalar: value)
  }

  /// Creates an instance initialized to a single Unicode scalar.
  ///
  /// Do not call this initializer directly. It may be used by the compiler
  /// when you initialize a static string with a Unicode scalar.
  @_effects(readonly)
  @_transparent
  public init(unicodeScalarLiteral value: StaticString) {
    self = value
  }

  @_effects(readonly)
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

  /// Creates an instance initialized to a single character that is made up of
  /// one or more Unicode scalar values.
  ///
  /// Do not call this initializer directly. It may be used by the compiler
  /// when you initialize a static string using an extended grapheme cluster.
  @_effects(readonly)
  @_transparent
  public init(extendedGraphemeClusterLiteral value: StaticString) {
    self = value
  }

  @_effects(readonly)
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

  /// Creates an instance initialized to the value of a string literal.
  ///
  /// Do not call this initializer directly. It may be used by the compiler
  /// when you initialize a static string using a string literal.
  @_effects(readonly)
  @_transparent
  public init(stringLiteral value: StaticString) {
    self = value
  }

  /// A string representation of the static string.
  public var description: String {
    return withUTF8Buffer { String._uncheckedFromUTF8($0) }
  }

  /// A textual representation of the static string, suitable for debugging.
  public var debugDescription: String {
    return self.description.debugDescription
  }
}

extension StaticString {
  public var customMirror: Mirror {
    return Mirror(reflecting: description)
  }
}
