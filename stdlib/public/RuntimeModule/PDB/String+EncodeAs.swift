//===--- String+EncodeAs.swift - PDB support for Swift --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Extends `String` to add support for encoding.
//
//===----------------------------------------------------------------------===//

import Swift

extension String {
  /// Calls a closure with a pointer to a buffer containing an encoded
  /// version of this string.
  ///
  /// - Parameters:
  ///   - encodedAs:   The encoding to use.
  ///   - trailingNul: If `true`, append a NUL to the end of the encoded result.
  ///   - body:        The closure to call.
  ///
  /// - Returns: The resutn value, if any, of the `body` closure parameeter.
  func withString<Result, TargetEncoding: Unicode.Encoding>(
    encodedAs targetEncoding: TargetEncoding.Type,
    trailingNul: Bool = false,
    _ body: (UnsafeBufferPointer<TargetEncoding.CodeUnit>) throws -> Result
  ) rethrows -> Result {
    var copy = self
    return try copy.withUTF8 { utf8 in
      var arg = Array<TargetEncoding.CodeUnit>()
      arg.reserveCapacity(1 &+ utf8.count / 4)
      let repaired = unsafe transcode(
        utf8.makeIterator(),
        from: UTF8.self,
        to: targetEncoding,
        stoppingOnError: false,
        into: { arg.append($0) })
      if trailingNul {
        arg.append(TargetEncoding.CodeUnit(0))
      }
      if repaired {
        fatalError("Encoding failed")
      }
      return try unsafe arg.withUnsafeBufferPointer {
        return try unsafe body($0)
      }
    }
  }

  /// Calls a closure with a pointer to a buffer containing the raw
  /// bytes of an encoded version of this string.
  ///
  /// - Parameters:
  ///   - encodedAs: The encoding to use.
  ///   - trailingNul: If `true`, append a NUL to the end of the encoded result.
  ///   - body:      The closure to call.
  ///
  /// - Returns: The resutn value, if any, of the `body` closure parameeter.
  func withBytes<Result, TargetEncoding: Unicode.Encoding>(
    encodedAs targetEncoding: TargetEncoding.Type,
    trailingNul: Bool = false,
    _ body: (UnsafeRawBufferPointer) throws -> Result
  ) rethrows -> Result {
    return try withString(encodedAs: targetEncoding) {
      return try unsafe body(UnsafeRawBufferPointer($0))
    }
  }
}
