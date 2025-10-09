//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import CxxStdlibShim

// MARK: Initializing C++ string from a Swift String

extension std.string {
  /// Creates a C++ string having the same content as the given Swift string.
  ///
  /// - Complexity: O(*n*), where *n* is the number of UTF-8 code units in the
  ///   Swift string.
  @_alwaysEmitIntoClient
  public init(_ string: String) {
    self = unsafe string.withCString(encodedAs: UTF8.self) { buffer in
      // MSVC STL has a enable_if template guard on the 3-parameter constructor,
      // and thus it's not imported into Swift.
      // libc++ provides both 2-parameter and 3-parameter constructors.
      // libstdc++ only provides the 3-parameter constructor.

      // Note that we might be compiling with libc++ on Linux, even if it's not
      // the default stdlib on a particular distro. 
#if os(Linux)
      unsafe std.string(buffer, string.utf8.count, .init())
#else
      unsafe std.string(buffer, string.utf8.count)
#endif
    }
  }


  @_alwaysEmitIntoClient
  @available(*, unavailable, message: "initializing std::string with an optional String is not supported; unwrap the optional value before passing it to std.string()")
  public init(_ string: String?) {
      fatalError("This initializer is unavailable and should never be called.")
  }

  @_alwaysEmitIntoClient
  public init(_ string: UnsafePointer<CChar>) {
#if os(Linux)
    unsafe self.init(string, UTF8._nullCodeUnitOffset(in: string), .init())
#else
    unsafe self.init(string, UTF8._nullCodeUnitOffset(in: string))
#endif
  }

  @_alwaysEmitIntoClient
  @_disfavoredOverload
  @available(*, deprecated, message: "unwrap the optional value and use init(_ string: UnsafePointer<CChar>) instead")
  public init(_ string: UnsafePointer<CChar>?) {
    guard let str = unsafe string else {
      self.init()
      return
    }
    unsafe self.init(str)
  }
}

extension std.u16string {
  /// Creates a C++ UTF-16 string having the same content as the given Swift
  /// string.
  ///
  /// - Complexity: O(*n*), where *n* is the number of UTF-16 code units in the
  ///   Swift string.
  @_alwaysEmitIntoClient
  public init(_ string: String) {
    self.init()
    for char in string.utf16 {
      self.push_back(char)
    }
  }
}

extension std.u32string {
  /// Creates a C++ UTF-32 string having the same content as the given Swift
  /// string.
  ///
  /// - Complexity: O(*n*), where *n* is the number of UTF-32 code units in the
  ///   Swift string.
  @_alwaysEmitIntoClient
  public init(_ string: String) {
    self.init()
    for char in string.unicodeScalars {
      self.push_back(char)
    }
  }
}

extension std.wstring {
  /// Creates a C++ wide character string having the same content as the given 
  /// Swift string.
  ///
  /// Note that the definition of a wide character differs across platforms:
  /// it is UTF-16 on Windows but UTF-32 on other platforms.
  ///
  /// - Complexity: O(*n*), where *n* is the number of wide characters in the
  ///   Swift string.
  @_alwaysEmitIntoClient
  public init(_ string: String) {
    self.init()
#if os(Windows)
    for char in string.utf16 {
      self.push_back(char)
    }
#else
    for char in string.unicodeScalars {
      self.push_back(char)
    }
#endif
  }
}

// MARK: Initializing C++ string from a Swift String literal

extension std.string: ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {

  @_alwaysEmitIntoClient
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension std.u16string: ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {

  @_alwaysEmitIntoClient
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension std.u32string: ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {

  @_alwaysEmitIntoClient
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension std.wstring: ExpressibleByStringLiteral,
  ExpressibleByStringInterpolation {

  @_alwaysEmitIntoClient
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

// MARK: Concatenating and comparing C++ strings

extension std.string: Equatable, Comparable {
  @_alwaysEmitIntoClient
  public static func ==(lhs: std.string, rhs: std.string) -> Bool {
    return lhs.compare(rhs) == 0
  }

  @_alwaysEmitIntoClient
  public static func <(lhs: std.string, rhs: std.string) -> Bool {
    return lhs.compare(rhs) < 0
  }

  @_alwaysEmitIntoClient
  public static func +=(lhs: inout std.string, rhs: std.string) {
    lhs.append(rhs)
  }

  @_alwaysEmitIntoClient
  public mutating func append(_ other: std.string) {
    unsafe __appendUnsafe(other) // ignore the returned pointer
  }

  @_alwaysEmitIntoClient
  public static func +(lhs: std.string, rhs: std.string) -> std.string {
    var copy = lhs
    copy += rhs
    return copy
  }
}

extension std.u16string: Equatable, Comparable {
  @_alwaysEmitIntoClient
  public static func ==(lhs: std.u16string, rhs: std.u16string) -> Bool {
    return lhs.compare(rhs) == 0
  }

  @_alwaysEmitIntoClient
  public static func <(lhs: std.u16string, rhs: std.u16string) -> Bool {
    return lhs.compare(rhs) < 0
  }

  @_alwaysEmitIntoClient
  public static func +=(lhs: inout std.u16string, rhs: std.u16string) {
    lhs.append(rhs)
  }

  @_alwaysEmitIntoClient
  public mutating func append(_ other: std.u16string) {
    unsafe __appendUnsafe(other) // ignore the returned pointer
  }

  @_alwaysEmitIntoClient
  public static func +(lhs: std.u16string, rhs: std.u16string) -> std.u16string {
    var copy = lhs
    copy += rhs
    return copy
  }
}

extension std.u32string: Equatable, Comparable {
  @_alwaysEmitIntoClient
  public static func ==(lhs: std.u32string, rhs: std.u32string) -> Bool {
    return lhs.compare(rhs) == 0
  }

  @_alwaysEmitIntoClient
  public static func <(lhs: std.u32string, rhs: std.u32string) -> Bool {
    return lhs.compare(rhs) < 0
  }

  @_alwaysEmitIntoClient
  public static func +=(lhs: inout std.u32string, rhs: std.u32string) {
    lhs.append(rhs)
  }

  @_alwaysEmitIntoClient
  public mutating func append(_ other: std.u32string) {
    unsafe __appendUnsafe(other) // ignore the returned pointer
  }

  @_alwaysEmitIntoClient
  public static func +(lhs: std.u32string, rhs: std.u32string) -> std.u32string {
    var copy = lhs
    copy += rhs
    return copy
  }
}

extension std.wstring: Equatable, Comparable {
  @_alwaysEmitIntoClient
  public static func ==(lhs: std.wstring, rhs: std.wstring) -> Bool {
    return lhs.compare(rhs) == 0
  }

  @_alwaysEmitIntoClient
  public static func <(lhs: std.wstring, rhs: std.wstring) -> Bool {
    return lhs.compare(rhs) < 0
  }

  @_alwaysEmitIntoClient
  public static func +=(lhs: inout std.wstring, rhs: std.wstring) {
    lhs.append(rhs)
  }

  @_alwaysEmitIntoClient
  public mutating func append(_ other: std.wstring) {
    unsafe __appendUnsafe(other) // ignore the returned pointer
  }

  @_alwaysEmitIntoClient
  public static func +(lhs: std.wstring, rhs: std.wstring) -> std.wstring {
    var copy = lhs
    copy += rhs
    return copy
  }
}

// MARK: Hashing C++ strings

extension std.string: Hashable {
  @_alwaysEmitIntoClient
  public func hash(into hasher: inout Hasher) {
    // Call std::hash<std::string>::operator()
    let cxxHash = __swift_interopComputeHashOfString(self)
    hasher.combine(cxxHash)
  }
}

extension std.u16string: Hashable {
  @_alwaysEmitIntoClient
  public func hash(into hasher: inout Hasher) {
    // Call std::hash<std::u16string>::operator()
    let cxxHash = __swift_interopComputeHashOfU16String(self)
    hasher.combine(cxxHash)
  }
}

extension std.u32string: Hashable {
  @_alwaysEmitIntoClient
  public func hash(into hasher: inout Hasher) {
    // Call std::hash<std::u32string>::operator()
    let cxxHash = __swift_interopComputeHashOfU32String(self)
    hasher.combine(cxxHash)
  }
}

extension std.wstring: Hashable {
  @_alwaysEmitIntoClient
  public func hash(into hasher: inout Hasher) {
    // Call std::hash<std::wstring>::operator()
    let cxxHash = __swift_interopComputeHashOfWString(self)
    hasher.combine(cxxHash)
  }
}

// MARK: Getting a Swift description of a C++ string

extension std.string: CustomDebugStringConvertible {
  @_alwaysEmitIntoClient
  public var debugDescription: String {
    return "std.string(\(String(self)))"
  }
}

extension std.u16string: CustomDebugStringConvertible {
  @_alwaysEmitIntoClient
  public var debugDescription: String {
    return "std.u16string(\(String(self)))"
  }
}

extension std.u32string: CustomDebugStringConvertible {
  @_alwaysEmitIntoClient
  public var debugDescription: String {
    return "std.u32string(\(String(self)))"
  }
}

extension std.wstring: CustomDebugStringConvertible {
  @_alwaysEmitIntoClient
  public var debugDescription: String {
    return "std.wstring(\(String(self)))"
  }
}

extension std.string: CustomStringConvertible {
  @_alwaysEmitIntoClient
  public var description: String {
    return String(self)
  }
}

extension std.u16string: CustomStringConvertible {
  @_alwaysEmitIntoClient
  public var description: String {
    return String(self)
  }
}

extension std.u32string: CustomStringConvertible {
  @_alwaysEmitIntoClient
  public var description: String {
    return String(self)
  }
}

extension std.wstring: CustomStringConvertible {
  @_alwaysEmitIntoClient
  public var description: String {
    return String(self)
  }
}

// MARK: Initializing Swift String from a C++ string

extension String {
  /// Creates a String having the same content as the given C++ string.
  ///
  /// If `cxxString` contains ill-formed UTF-8 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of bytes in the C++ string.
  @_alwaysEmitIntoClient
  public init(_ cxxString: std.string) {
    let buffer = unsafe UnsafeBufferPointer<CChar>(
      start: cxxString.__c_strUnsafe(),
      count: cxxString.size())
    self = unsafe buffer.withMemoryRebound(to: UInt8.self) {
      unsafe String(decoding: $0, as: UTF8.self)
    }
    withExtendedLifetime(cxxString) {}
  }

  /// Creates a String having the same content as the given C++ UTF-16 string.
  ///
  /// If `cxxString` contains ill-formed UTF-16 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of bytes in the C++ UTF-16
  ///   string.
  @_alwaysEmitIntoClient
  public init(_ cxxU16String: std.u16string) {
    let buffer = unsafe UnsafeBufferPointer<UInt16>(
      start: cxxU16String.__dataUnsafe(),
      count: cxxU16String.size())
    self = unsafe String(decoding: buffer, as: UTF16.self)
    withExtendedLifetime(cxxU16String) {}
  }

  /// Creates a String having the same content as the given C++ UTF-32 string.
  ///
  /// If `cxxString` contains ill-formed UTF-32 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of bytes in the C++ UTF-32
  ///   string.
  @_alwaysEmitIntoClient
  public init(_ cxxU32String: std.u32string) {
    let buffer = unsafe UnsafeBufferPointer<Unicode.Scalar>(
      start: cxxU32String.__dataUnsafe(),
      count: cxxU32String.size())
    self = unsafe buffer.withMemoryRebound(to: UInt32.self) {
      unsafe String(decoding: $0, as: UTF32.self)
    }
    withExtendedLifetime(cxxU32String) {}
  }

  /// Creates a String having the same content as the given C++ wide character 
  /// string.
  ///
  /// Note that the definition of a wide character differs across platforms:
  /// it is UTF-16 on Windows but UTF-32 on other platforms.
  ///
  /// If `cxxString` contains ill-formed UTF code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of wide characters in the
  ///   C++ string.
  @_alwaysEmitIntoClient
  public init(_ cxxWString: std.wstring) {
#if os(Windows)
    let buffer = unsafe UnsafeBufferPointer<UInt16>(
      start: cxxWString.__dataUnsafe(),
      count: cxxWString.size())
    self = unsafe String(decoding: buffer, as: UTF16.self)
#else
    let buffer = unsafe UnsafeBufferPointer<Unicode.Scalar>(
      start: cxxWString.__dataUnsafe(),
      count: cxxWString.size())
    self = unsafe buffer.withMemoryRebound(to: UInt32.self) {
      unsafe String(decoding: $0, as: UTF32.self)
    }
#endif
    withExtendedLifetime(cxxWString) {}
  }
}

// MARK: Initializing Swift String from a C++ string_view

extension String {
  /// Creates a String having the same content as the given C++ string view.
  ///
  /// If `cxxStringView` contains ill-formed UTF-8 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of bytes in the C++ string
  ///   view.
  @_alwaysEmitIntoClient
  public init(_ cxxStringView: std.string_view) {
    let buffer = unsafe UnsafeBufferPointer<CChar>(
      start: cxxStringView.__dataUnsafe(),
      count: cxxStringView.size())
    self = unsafe buffer.withMemoryRebound(to: UInt8.self) {
      unsafe String(decoding: $0, as: UTF8.self)
    }
    unsafe withExtendedLifetime(cxxStringView) {}
  }

  /// Creates a String having the same content as the given C++ UTF-16 string
  /// view.
  ///
  /// If `cxxU16StringView` contains ill-formed UTF-16 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of bytes in the C++ UTF-16
  ///   string view.
  @_alwaysEmitIntoClient
  public init(_ cxxU16StringView: std.u16string_view) {
    let buffer = unsafe UnsafeBufferPointer<UInt16>(
      start: cxxU16StringView.__dataUnsafe(),
      count: cxxU16StringView.size())
    self = unsafe String(decoding: buffer, as: UTF16.self)
    unsafe withExtendedLifetime(cxxU16StringView) {}
  }

  /// Creates a String having the same content as the given C++ UTF-32 string
  /// view.
  ///
  /// If `cxxU32StringView` contains ill-formed UTF-32 code unit sequences, this
  /// initializer replaces them with the Unicode replacement character
  /// (`"\u{FFFD}"`).
  ///
  /// - Complexity: O(*n*), where *n* is the number of bytes in the C++ UTF-32
  ///   string view.
  @_alwaysEmitIntoClient
  public init(_ cxxU32StringView: std.u32string_view) {
    let buffer = unsafe UnsafeBufferPointer<Unicode.Scalar>(
      start: cxxU32StringView.__dataUnsafe(),
      count: cxxU32StringView.size())
    self = unsafe buffer.withMemoryRebound(to: UInt32.self) {
      unsafe String(decoding: $0, as: UTF32.self)
    }
    unsafe withExtendedLifetime(cxxU32StringView) {}
  }

  @_alwaysEmitIntoClient
  public init(_ cxxWStringView: std.wstring_view) {
#if os(Windows)
    let buffer = unsafe UnsafeBufferPointer<UInt16>(
      start: cxxWStringView.__dataUnsafe(),
      count: cxxWStringView.size())
    self = unsafe String(decoding: buffer, as: UTF16.self)
#else
    let buffer = unsafe UnsafeBufferPointer<Unicode.Scalar>(
      start: cxxWStringView.__dataUnsafe(),
      count: cxxWStringView.size())
    self = unsafe buffer.withMemoryRebound(to: UInt32.self) {
      unsafe String(decoding: $0, as: UTF32.self)
    }
#endif
    unsafe withExtendedLifetime(cxxWStringView) {}
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
extension std.string {
  public var span: Span<CChar> {
    @_lifetime(borrow self)
    @_alwaysEmitIntoClient
    borrowing get {
      let buffer = unsafe UnsafeBufferPointer(start: self.__dataUnsafe(), count: Int(self.size()))
      let span = unsafe Span(_unsafeElements: buffer)
      return unsafe _cxxOverrideLifetime(span, borrowing: self)
    }
  }
}

@available(SwiftStdlib 6.2, *)
extension std.string {
  public var utf8Span: UTF8Span? {
    @_lifetime(borrow self)
    @_alwaysEmitIntoClient
    borrowing get {
      let buffer = unsafe UnsafeBufferPointer(start: self.__dataUnsafe(), count: Int(self.size()))
      let rawBuffer = UnsafeRawBufferPointer(buffer)
      let bufferWithFixedType = unsafe rawBuffer.assumingMemoryBound(to: UInt8.self)
      let span = unsafe Span(_unsafeElements: bufferWithFixedType)
      let spanWithFixedLifetime = unsafe _cxxOverrideLifetime(span, borrowing: self)
      return try? UTF8Span(validating: spanWithFixedLifetime)
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
extension std.u16string {
  public var span: Span<UInt16> {
    @_lifetime(borrow self)
    @_alwaysEmitIntoClient
    borrowing get {
      let buffer = unsafe UnsafeBufferPointer(start: self.__dataUnsafe(), count: Int(self.size()))
      let rawBuffer = UnsafeRawBufferPointer(buffer)
      let bufferWithFixedType = unsafe rawBuffer.assumingMemoryBound(to: UInt16.self)
      let span = unsafe Span(_unsafeElements: bufferWithFixedType)
      return unsafe _cxxOverrideLifetime(span, borrowing: self)
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
extension std.u32string {
  public var span: Span<UInt32> {
    @_lifetime(borrow self)
    @_alwaysEmitIntoClient
    borrowing get {
      let buffer = unsafe UnsafeBufferPointer(start: self.__dataUnsafe(), count: Int(self.size()))
      let rawBuffer = UnsafeRawBufferPointer(buffer)
      let bufferWithFixedType = unsafe rawBuffer.assumingMemoryBound(to: UInt32.self)
      let span = unsafe Span(_unsafeElements: bufferWithFixedType)
      return unsafe _cxxOverrideLifetime(span, borrowing: self)
    }
  }
}

@available(SwiftCompatibilitySpan 5.0, *)
extension std.wstring {
  public var span: Span<CWideChar> {
    @_lifetime(borrow self)
    @_alwaysEmitIntoClient
    borrowing get {
      let buffer = unsafe UnsafeBufferPointer(start: self.__dataUnsafe(), count: Int(self.size()))
      let rawBuffer = UnsafeRawBufferPointer(buffer)
      let bufferWithFixedType = unsafe rawBuffer.assumingMemoryBound(to: CWideChar.self)
      let span = unsafe Span(_unsafeElements: bufferWithFixedType)
      return unsafe _cxxOverrideLifetime(span, borrowing: self)
    }
  }
}
