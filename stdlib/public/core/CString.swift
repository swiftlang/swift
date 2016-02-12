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
// String interop with C
//===----------------------------------------------------------------------===//

import SwiftShims

extension String {
  /// Creates a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `CString`.
  ///
  /// Returns `nil` if the `CString` is `NULL` or if it contains ill-formed
  /// UTF-8 code unit sequences.
  @warn_unused_result
  public static func fromCString(cs: UnsafePointer<CChar>) -> String? {
    if cs._isNull {
      return nil
    }
    let len = Int(_swift_stdlib_strlen(cs))
    return String(validatingCodeUnits: UnsafeBufferPointer(
        start: UnsafePointer(cs),
        count: len),
      as: UTF8.self)
  }

  /// Creates a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `CString`.
  ///
  /// Returns `nil` if the `CString` is `NULL`.  If `CString` contains
  /// ill-formed UTF-8 code unit sequences, replaces them with replacement
  /// characters (U+FFFD).
  @warn_unused_result
  public static func fromCStringRepairingIllFormedUTF8(
    cs: UnsafePointer<CChar>)
      -> (String?, hadError: Bool) {
    if cs._isNull {
      return (nil, hadError: false)
    }
    let len = Int(_swift_stdlib_strlen(cs))
    let buffer = UnsafeBufferPointer<UTF8.CodeUnit>(
      start: UnsafePointer(cs), count: len)

    guard let (result, hadError) = String._decode(buffer, as: UTF8.self) else {
      return (nil, hadError: false)
    }
    return (result, hadError: hadError)
  }
}

/// From a non-`nil` `UnsafePointer` to a null-terminated string
/// with possibly-transient lifetime, create a null-terminated array of 'C' char.
/// Returns `nil` if passed a null pointer.
@warn_unused_result
public func _persistCString(s: UnsafePointer<CChar>) -> [CChar]? {
  if s == nil {
    return nil
  }
  let length = Int(_swift_stdlib_strlen(s))
  var result = [CChar](count: length + 1, repeatedValue: 0)
  for i in 0..<length {
    result[i] = s[i]
  }
  return result
}
