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
  /// - Precondition: `cString != nil`
  public init(cString: UnsafePointer<CChar>) {
    self = String.decodeCString(UnsafePointer(cString), as: UTF8.self,
      repairingInvalidCodeUnits: true)!.result
  }

  /// Create a new `String` by copying the nul-terminated UTF-8 data
  /// referenced by a `cString`.
  ///
  /// Does not try to repair ill-formed UTF-8 code unit sequences, fails if any
  /// such sequences are found.
  ///
  /// - Precondition: `cString != nil`
  public init?(validatingUTF8 cString: UnsafePointer<CChar>) {
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
  /// Returns `nil` if the `cString` is `nil` or if it contains ill-formed code
  /// units and no repairing has been requested. Otherwise replaces
  /// ill-formed code units with replacement characters (U+FFFD).
  @warn_unused_result
  public static func decodeCString<Encoding : UnicodeCodec>(
    _ cString: UnsafePointer<Encoding.CodeUnit>?,
    as encoding: Encoding.Type,
    repairingInvalidCodeUnits isRepairing: Bool = true)
      -> (result: String, repairsMade: Bool)? {

    guard let cString = cString else {
      return nil
    }
    let len = Int(_swift_stdlib_strlen(UnsafePointer(cString)))
    let buffer = UnsafeBufferPointer<Encoding.CodeUnit>(
      start: cString, count: len)

    let (stringBuffer, hadError) = _StringBuffer.fromCodeUnits(
      buffer, encoding: encoding, repairIllFormedSequences: isRepairing)
    return stringBuffer.map {
      (result: String(_storage: $0), repairsMade: hadError)
    }
  }

}

/// From a non-`nil` `UnsafePointer` to a null-terminated string
/// with possibly-transient lifetime, create a null-terminated array of 'C' char.
/// Returns `nil` if passed a null pointer.
@warn_unused_result
public func _persistCString(_ p: UnsafePointer<CChar>?) -> [CChar]? {
  guard let s = p else {
    return nil
  }
  let count = Int(_swift_stdlib_strlen(s))
  var result = [CChar](repeating: 0, count: count + 1)
  for i in 0..<count {
    result[i] = s[i]
  }
  return result
}

extension String {
  @available(*, unavailable, message: "Please use String.init?(validatingUTF8:) instead. Note that it no longer accepts NULL as a valid input. Also consider using String(cString:), that will attempt to repair ill-formed code units.")
  @warn_unused_result
  public static func fromCString(_ cs: UnsafePointer<CChar>) -> String? {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Please use String.init(cString:) instead. Note that it no longer accepts NULL as a valid input. See also String.decodeCString if you need more control.")
  @warn_unused_result
  public static func fromCStringRepairingIllFormedUTF8(
    _ cs: UnsafePointer<CChar>
  ) -> (String?, hadError: Bool) {
    Builtin.unreachable()
  }
}
