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

// MARK: Initializing C++ string from a Swift String

extension std.string {
  public init(_ string: String) {
    self.init()
    for char in string.utf8 {
      self.push_back(value_type(bitPattern: char))
    }
  }
}

extension std.u16string {
  public init(_ string: String) {
    self.init()
    for char in string.utf16 {
      self.push_back(char)
    }
  }
}

// MARK: Initializing C++ string from a Swift String literal

extension std.string: ExpressibleByStringLiteral {
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension std.u16string: ExpressibleByStringLiteral {
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

// MARK: Initializing Swift String from a C++ string

extension String {
  public init(cxxString: std.string) {
    let buffer = UnsafeBufferPointer<CChar>(
      start: cxxString.__c_strUnsafe(),
      count: cxxString.size())
    self = buffer.withMemoryRebound(to: UInt8.self) {
      String(decoding: $0, as: UTF8.self)
    }
    withExtendedLifetime(cxxString) {}
  }

  public init(cxxU16String: std.u16string) {
    let buffer = UnsafeBufferPointer<UInt16>(
      start: cxxU16String.__dataUnsafe(),
      count: cxxU16String.size())
    self = String(decoding: buffer, as: UTF16.self)
    withExtendedLifetime(cxxU16String) {}
  }
}
