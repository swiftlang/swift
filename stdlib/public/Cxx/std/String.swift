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
    self = string.withCString(encodedAs: UTF8.self) { buffer in
#if os(Windows)
      // Use the 2 parameter constructor.
      // The MSVC standard library has a enable_if template guard
      // on the 3 parameter constructor, and thus it's not imported into Swift.
      std.string(buffer, string.utf8.count)
#else
      std.string(buffer, string.utf8.count, .init())
#endif
    }
  }

  @_alwaysEmitIntoClient
  public init(_ string: UnsafePointer<CChar>?) {
    if let str = string {
#if os(Windows)
      // Use the 2 parameter constructor.
      // The MSVC standard library has a enable_if template guard
      // on the 3 parameter constructor, and thus it's not imported into Swift.
      self.init(str, UTF8._nullCodeUnitOffset(in: str))
#else
      self.init(str, UTF8._nullCodeUnitOffset(in: str), .init())
#endif
    } else {
      self.init()
    }
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

// MARK: Initializing C++ string from a Swift String literal

extension std.string: ExpressibleByStringLiteral {
  @_alwaysEmitIntoClient
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension std.u16string: ExpressibleByStringLiteral {
  @_alwaysEmitIntoClient
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension std.u32string: ExpressibleByStringLiteral {
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
    __appendUnsafe(other) // ignore the returned pointer
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
    __appendUnsafe(other) // ignore the returned pointer
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
    __appendUnsafe(other) // ignore the returned pointer
  }

  @_alwaysEmitIntoClient
  public static func +(lhs: std.u32string, rhs: std.u32string) -> std.u32string {
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
    let buffer = UnsafeBufferPointer<CChar>(
      start: cxxString.__c_strUnsafe(),
      count: cxxString.size())
    self = buffer.withMemoryRebound(to: UInt8.self) {
      String(decoding: $0, as: UTF8.self)
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
    let buffer = UnsafeBufferPointer<UInt16>(
      start: cxxU16String.__dataUnsafe(),
      count: cxxU16String.size())
    self = String(decoding: buffer, as: UTF16.self)
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
    let buffer = UnsafeBufferPointer<Unicode.Scalar>(
      start: cxxU32String.__dataUnsafe(),
      count: cxxU32String.size())
    self = buffer.withMemoryRebound(to: UInt32.self) {
      String(decoding: $0, as: UTF32.self)
    }
    withExtendedLifetime(cxxU32String) {}
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
    let buffer = UnsafeBufferPointer<CChar>(
      start: cxxStringView.__dataUnsafe(),
      count: cxxStringView.size())
    self = buffer.withMemoryRebound(to: UInt8.self) {
      String(decoding: $0, as: UTF8.self)
    }
    withExtendedLifetime(cxxStringView) {}
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
    let buffer = UnsafeBufferPointer<UInt16>(
      start: cxxU16StringView.__dataUnsafe(),
      count: cxxU16StringView.size())
    self = String(decoding: buffer, as: UTF16.self)
    withExtendedLifetime(cxxU16StringView) {}
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
    let buffer = UnsafeBufferPointer<Unicode.Scalar>(
      start: cxxU32StringView.__dataUnsafe(),
      count: cxxU32StringView.size())
    self = buffer.withMemoryRebound(to: UInt32.self) {
      String(decoding: $0, as: UTF32.self)
    }
    withExtendedLifetime(cxxU32StringView) {}
  }
}
