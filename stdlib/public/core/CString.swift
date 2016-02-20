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

  /// Create a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `cString`.
  ///
  /// If `cString` contains ill-formed UTF-8 code unit sequences, replaces them
  /// with replacement characters (U+FFFD).
  ///
  /// - Requires: `cString != nil`
  public init(cString: UnsafePointer<CChar>) {
    _precondition(cString != nil, "cString must not be nil")
    self = String.decodeCString(UnsafePointer(cString), as: UTF8.self,
      repairingInvalidCodeUnits: true)!.result
  }

  /// Create a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `cString`.
  ///
  /// Does not try to repair ill-formed UTF-8 code unit sequences, fails if any
  /// such sequences are found.
  ///
  /// - Requires: `cString != nil`
  public init?(validatingUTF8 cString: UnsafePointer<CChar>) {
    _precondition(cString != nil, "cString must not be nil")
    guard let (result, _) = String.decodeCString(
      UnsafePointer(cString),
        as: UTF8.self,
        repairingInvalidCodeUnits: false) else {
      return nil
    }
    self = result
  }

  /// Create a new `String` by copying the nul-terminated data
  /// referenced by a `cString` using `encoding`.
  ///
  /// Returns `nil` if the `cString` is `NULL` or if it contains ill-formed code
  /// units and no repairing has been requested. Otherwise replaces
  /// ill-formed code units with replacement characters (U+FFFD).
  @warn_unused_result
  public static func decodeCString<Encoding : UnicodeCodec>(
    cString: UnsafePointer<Encoding.CodeUnit>,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isReparing: Bool = true)
      -> (result: String, repairsMade: Bool)? {

    if cString._isNull {
      return nil
    }
    let len = Int(_swift_stdlib_strlen(UnsafePointer(cString)))
    let buffer = UnsafeBufferPointer<Encoding.CodeUnit>(
      start: cString, count: len)

    let (stringBuffer, hadError) = _StringBuffer.fromCodeUnits(
      encoding, input: buffer, repairIllFormedSequences: isReparing)
    return stringBuffer.map {
      (result: String(_storage: $0), repairsMade: hadError)
    }
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
  let count = Int(_swift_stdlib_strlen(s))
  var result = [CChar](repeating: 0, count: count + 1)
  for i in 0..<count {
    result[i] = s[i]
  }
  return result
}
