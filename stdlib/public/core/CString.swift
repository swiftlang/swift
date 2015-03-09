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
// String interop with C
//===----------------------------------------------------------------------===//

import SwiftShims // for strlen, strcpy, strcmp

extension String {
  /// Creates a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `CString`.
  ///
  /// Returns `nil` if the `CString` is `NULL` or if it contains ill-formed
  /// UTF-8 code unit sequences.
  public static func fromCString(cs: UnsafePointer<CChar>) -> String? {
    if cs._isNull {
      return .None
    }
    let len = Int(strlen(cs))
    return String._fromCodeUnitSequence(UTF8.self,
        input: UnsafeBufferPointer(start: UnsafeMutablePointer(cs), count: len))
  }

  /// Creates a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `CString`.
  ///
  /// Returns `nil` if the `CString` is `NULL`.  If `CString` contains
  /// ill-formed UTF-8 code unit sequences, replaces them with replacement
  /// characters (U+FFFD).
  public static func fromCStringRepairingIllFormedUTF8(
    cs: UnsafePointer<CChar>)
      -> (String?, hadError: Bool) {
    if cs._isNull {
      return (.None, hadError: false)
    }
    let len = Int(strlen(cs))
    let (result, hadError) = String._fromCodeUnitSequenceWithRepair(UTF8.self,
        input: UnsafeBufferPointer(start: UnsafeMutablePointer(cs), count: len))
    return (result, hadError: hadError)
  }
}

/// From a non-`nil` `UnsafePointer` to a null-terminated string
/// with possibly-transient lifetime, create a nul-terminated array of 'C' char.
/// Returns `nil` if passed a null pointer.
public func _persistCString(s: UnsafePointer<CChar>) -> [CChar]? {
  if s == nil {
    return .None
  }
  var length = Int(strlen(s))
  var result = [CChar](count: length + 1, repeatedValue: 0)
  for var i = 0; i < length; ++i {
    // FIXME: this will not compile on platforms where 'CChar' is unsigned.
    result[i] = s[i]
  }
  return result
}
